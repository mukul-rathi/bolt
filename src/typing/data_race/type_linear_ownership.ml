open Core
open Result
open Ast.Ast_types
open Typing_core_lang.Typed_ast
open Typing_core_lang.Type_env

(* Linear references can be "owned" (bound to a ref) or "free" (not bound to a ref) *)
type ownership = LinearOwned | LinearFree | NonLinear

let has_linear_cap type_expr class_defns loc =
  match get_type_capability type_expr class_defns loc with
  | Ok Linear                     -> true
  | Ok Read | Ok Thread | Error _ -> false

(* Explanation for why this algorithm actually checks linear ownership.

   Linear ownership is only violated if we try to alias an owned linear type.

   let x = new LinClass() in // fine since new linear object is free let y = x in // not
   fine, since linear object owned by x ... We recurse by checking ownership of the final
   value the expression will reduce to - since that is what will be aliased in the let
   production. *)

(* Helper function used to return results of subcomputations when recursively type-check
   expression (top level function returns unit) *)
let rec type_linear_ownership_helper class_defns trait_defns function_defns expr =
  let type_linear_ownership_with_defns =
    type_linear_ownership_helper class_defns trait_defns function_defns in
  match expr with
  | Integer (_, _) -> Ok NonLinear
  | Variable (loc, var_type, _) ->
      if has_linear_cap var_type class_defns loc then Ok LinearOwned else Ok NonLinear
  | App (loc, _, func_name, args_exprs) ->
      get_function_body_expr func_name function_defns loc
      >>= fun function_body_expr ->
      Result.all_unit
        (List.map
           ~f:(fun arg_expr ->
             Result.ignore_m (type_linear_ownership_with_defns arg_expr))
           args_exprs)
      (* during application the arg will be subbed into the func expr, so we care about
         what the func expr will reduce to - since that'll be the final value *)
      >>= fun () -> type_linear_ownership_with_defns function_body_expr
  | Block (_, _, exprs) ->
      List.fold ~init:(Ok NonLinear)
        ~f:(fun acc expr ->
          Result.ignore_m acc >>= fun () -> type_linear_ownership_with_defns expr)
        (* Recurse on each expression but only take value of last expression in block *)
        exprs
  | ObjField (loc, expr_type, _, var_type, _) ->
      (* Check if either the object or its field are linear references *)
      if
        has_linear_cap var_type class_defns loc
        || has_linear_cap expr_type class_defns loc
      then Ok LinearOwned
      else Ok NonLinear
  | Assign (loc, expr_type, _, var_type, _, assigned_expr) ->
      Result.ignore_m (type_linear_ownership_with_defns assigned_expr)
      (* We don't care about the result since it is being assigned i.e. potentially owned *)
      >>= fun () ->
      if
        (* Check if either the object or its field are linear references *)
        has_linear_cap var_type class_defns loc
        || has_linear_cap expr_type class_defns loc
      then Ok LinearOwned
      else Ok NonLinear
  | Constructor (loc, expr_type, _, constructor_args) ->
      (* Recurse on constructor args, but we don't care about results as we're assigning
         the overall constructed object, not the args *)
      Result.ignore_m
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
  | Consume (_, _, expr) -> (
      type_linear_ownership_with_defns expr
      >>| function
      | LinearOwned | LinearFree ->
          LinearFree (* consuming an expression frees ownership *)
      | NonLinear                -> NonLinear )
  | FinishAsync (_, _, async_expr1, async_expr2, next_expr) ->
      Result.ignore_m (type_linear_ownership_with_defns async_expr1)
      (* this expression will by reduced to the next_expr's value so we don't care about
         the ownership of the async exprs *)
      >>= fun () ->
      Result.ignore_m (type_linear_ownership_with_defns async_expr2)
      >>= fun () -> type_linear_ownership_with_defns next_expr
  | Let (loc, _, _, bound_expr) -> (
      type_linear_ownership_with_defns bound_expr
      >>= function
      | LinearFree  -> Ok LinearOwned (* We've now captured the linear expression *)
      | NonLinear   -> Ok NonLinear
      | LinearOwned ->
          Error
            (Error.of_string
               (Fmt.str "%s Potential data race: aliasing a linear reference@."
                  (string_of_loc loc))) )

let type_functions_linear_ownership class_defns trait_defns function_defns =
  Result.all_unit
    (List.map
       ~f:(fun (TFunction (_, _, _, body_expr)) ->
         Result.ignore_m
           (type_linear_ownership_helper class_defns trait_defns function_defns body_expr))
       function_defns)

(* top level expression to return - we discard the value used in recursive subcomputation *)
let type_linear_ownership (Prog (class_defns, trait_defns, function_defns, expr)) =
  type_functions_linear_ownership class_defns trait_defns function_defns
  >>= fun () ->
  Result.ignore_m
    (type_linear_ownership_helper class_defns trait_defns function_defns expr)
