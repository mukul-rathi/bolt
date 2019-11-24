open Core
open Result
open Ast_types
open Typed_ast
open Type_env

(* Linear references can be "owned" (bound to a ref) or "free" (not bound to a ref) *)
type ownership = LinearOwned | LinearFree | NonLinear

let has_linear_cap type_expr class_defns loc =
  match get_type_capability type_expr class_defns loc with
  | Ok Linear -> true
  | Ok Read | Ok Thread | Error _ -> false

(* Explanation for why this algorithm actually checks linear ownership.

   Linear ownership is only violated if we try to alias an owned linear type.

   let x = new LinClass() in // fine since new linear object is free let y = x in // not
   fine, since linear object owned by x ... We recurse by checking ownership of the final
   value the expression will reduce to - since that is what will be aliased in the let
   production. *)

let rec type_linear_ownership class_defns trait_defns expr =
  let type_linear_ownership_with_defns = type_linear_ownership class_defns trait_defns in
  match expr with
  | Integer (_, _)                                          -> Ok NonLinear
  | Variable (loc, var_type, _)                             ->
      if has_linear_cap var_type class_defns loc then Ok LinearOwned else Ok NonLinear
  | Lambda (_, _, _, _, body_expr)                          -> type_linear_ownership_with_defns
                                                                 body_expr
  | App (_, _, func_expr, arg_expr)                         ->
      Result.ignore (type_linear_ownership_with_defns arg_expr)
      (* during application the arg will be subbed into the func expr, so we care about
         what the func expr will reduce to - since that'll be the final value *)
      >>= fun () -> type_linear_ownership_with_defns func_expr
  | Seq (_, _, exprs)                                       ->
      List.fold ~init:(Ok NonLinear)
        ~f:(fun acc expr ->
          Result.ignore acc >>= fun () -> type_linear_ownership_with_defns expr)
        (* Recurse on each expression but only take value of last expression in sequence*)
        exprs
  | ObjField (loc, expr_type, _, var_type, _)               ->
      (* Check if either the object or its field are linear references *)
      if
        has_linear_cap var_type class_defns loc
        || has_linear_cap expr_type class_defns loc
      then Ok LinearOwned
      else Ok NonLinear
  | Assign (loc, expr_type, _, var_type, _, assigned_expr)  ->
      Result.ignore (type_linear_ownership_with_defns assigned_expr)
      (* We don't care about the result since it is being assigned i.e. potentially owned *)
      >>= fun () ->
      if
        (* Check if either the object or its field are linear references *)
        has_linear_cap var_type class_defns loc
        || has_linear_cap expr_type class_defns loc
      then Ok LinearOwned
      else Ok NonLinear
  | Constructor (loc, expr_type, _, constructor_args)       ->
      (* Recurse on constructor args, but we don't care about results as we're assigning
         the overall constructed object, not the args *)
      Result.ignore
        (Result.all
           (List.map
              ~f:(fun (ConstructorArg (_, _, expr)) ->
                type_linear_ownership_with_defns expr)
              constructor_args))
      >>= fun () ->
      if
        (* Check if the object being constructed is linear *)
        has_linear_cap expr_type class_defns loc
      then Ok LinearFree
      else Ok NonLinear
  | Consume (_, _, expr)                                    -> (
      type_linear_ownership_with_defns expr
      >>| function
      | LinearOwned | LinearFree ->
          LinearFree (* consuming an expression frees ownership *)
      | NonLinear -> NonLinear )
  | FinishAsync (_, _, async_expr1, async_expr2, next_expr) ->
      Result.ignore (type_linear_ownership_with_defns async_expr1)
      (* this expression will by reduced to the next_expr's value so we don't care about
         the ownership of the async exprs *)
      >>= fun () ->
      Result.ignore (type_linear_ownership_with_defns async_expr2)
      >>= fun () -> type_linear_ownership_with_defns next_expr
  | Let (loc, _, _, subbed_expr, body_expr)                 -> (
      Result.ignore (type_linear_ownership_with_defns body_expr)
      (* Recurse on body, but really, we care about checking the binding of the subbed
         expression *)
      >>= fun () ->
      type_linear_ownership_with_defns subbed_expr
      >>= function
      | LinearFree  -> Ok LinearOwned (* We've now captured the linear expression *)
      | NonLinear   -> Ok NonLinear
      | LinearOwned ->
          Error
            (Error.of_string
               (Fmt.str "%s Potential data race: aliasing a linear reference@."
                  (string_of_loc loc))) )
