open Core
open Result
open Ast.Ast_types
open Typing_core_lang.Typed_ast
open Data_race_type_env

let remove_bound_var bound_var_name env =
  List.filter ~f:(fun (var_name, _) -> not (var_name = bound_var_name)) env

let union_envs env_list =
  List.dedup_and_sort
    ~compare:(fun (name_1, _) (name_2, _) -> if name_1 = name_2 then 0 else 1)
    (List.concat env_list)

let has_read_or_no_cap type_expr class_defns loc =
  match get_type_capability type_expr class_defns loc with
  | Ok Read | Error _     -> true
  | Ok Linear | Ok Thread -> false

let envs_only_share_read_vars env1 env2 class_defns loc =
  not
    (List.contains_dup
       ~compare:(fun (name_1, type_1) (name_2, type_2) ->
         if name_1 = name_2 then
           (* We are sharing a var *)
           if
             has_read_or_no_cap type_1 class_defns loc
             && has_read_or_no_cap type_2 class_defns loc
           then 1 (* Ok to share *)
           else 0 (* Can't share var *)
         else 1) (* Ok as distinct *)
       (env1 @ env2))

let env_contains_thread_vars env class_defns loc =
  List.exists
    ~f:(fun (_, var_type) ->
      match get_type_capability var_type class_defns loc with
      | Ok Thread                     -> true
      | Ok Linear | Ok Read | Error _ -> false
      (* no / other capabilities *))
    env

(* Explanation for why this algorithm actually types async exprs.

   Typing async exprs entails two things:

   1) Check that the environments of two async expressions are disjoint modulo immutable
   (i.e. read/no cap) references. Note that this covers the case where there is variable
   shadowing (e.g. x ) since both async exprs would refer to the same (most-inner scope)
   x, so behaviour is identical to no shadowing.

   2) Any thread variables declared in preceding code does not get referenced in the
   second expression (conceptually a forked thread from the first expr).

   Rather than running type-checking and keeping track of the environment calls made
   during type inference of each async expression, instead note that because we pass
   type-checking, the overall program expression is closed, so if we consider an subexpr
   in isolation, any free variables are variables that were declared earlier in the
   program. Bound variables in async exprs are not in the scope of the other async expr so
   aren't seen.

   So 1) amounts to checking that fv(expr1) intersect fv(expr2) = ø and 2) amounts to
   checking no free vars in expr2 have type "thread". (Note that any thread variables
   created in second thread are bound, so free "thread" vars must have been from another
   thread.) *)

(* Helper function used to return results of subcomputations when recursively type-check
   expression (top level function returns unit) *)
let rec type_async_expr_helper class_defns trait_defns expr =
  let type_async_expr_with_defns = type_async_expr_helper class_defns trait_defns in
  match expr with
  | Unit _ -> Ok []
  | Integer (_, _) -> Ok []
  | Variable (_, var_type, var_name) -> Ok [(var_name, var_type)]
  | App (_, _, _, args_exprs) ->
      Result.all (List.map ~f:type_async_expr_with_defns args_exprs) >>| union_envs
  | Block (loc, block_type, block_exprs) -> (
    match block_exprs with
    | []            -> Ok []
    | expr :: exprs -> (
        type_async_expr_with_defns (Block (loc, block_type, exprs))
        >>= fun exprs_free_vars ->
        match expr with
        (* If let binding then need to remove bound variable from block's free vars *)
        | Let (_, _, var_name, bound_expr) ->
            type_async_expr_with_defns bound_expr
            >>| fun bound_expr_free_vars ->
            union_envs [bound_expr_free_vars; remove_bound_var var_name exprs_free_vars]
        | _ ->
            type_async_expr_with_defns expr
            >>| fun expr_free_vars -> union_envs [expr_free_vars; exprs_free_vars] ) )
  | Let (_, _, _, bound_expr) -> type_async_expr_with_defns bound_expr
  | ObjField (_, _, var_name, var_type, _) -> Ok [(var_name, var_type)]
  | ObjMethod (_, _, _, _, _, args_exprs) ->
      Result.all (List.map ~f:type_async_expr_with_defns args_exprs) >>| union_envs
  | Assign (_, _, var_name, var_type, _, assigned_expr) ->
      type_async_expr_with_defns assigned_expr
      >>| fun assign_expr_env -> union_envs [assign_expr_env; [(var_name, var_type)]]
  | Constructor (_, _, _, constructor_args) ->
      Result.all
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) -> type_async_expr_with_defns expr)
           constructor_args)
      >>| union_envs
  | Consume (_, _, expr) -> type_async_expr_with_defns expr
  | FinishAsync (loc, _, async_expr1, async_expr2, next_expr) ->
      type_async_expr_with_defns async_expr1
      >>= fun async_expr1_env ->
      type_async_expr_with_defns async_expr2
      >>= fun async_expr2_env ->
      if not (envs_only_share_read_vars async_expr1_env async_expr2_env class_defns loc)
      then
        Error
          (Error.of_string
             (Fmt.str "%s Potential data race: threads share mutable variables.@."
                (string_of_loc loc)))
      else if env_contains_thread_vars async_expr2_env class_defns loc then
        Error
          (Error.of_string
             (Fmt.str
                "%s Potential data race: thread-local variable accessed from other thread.@."
                (string_of_loc loc)))
      else
        (* We're good, so check sub-exprs *)
        type_async_expr_with_defns next_expr
        >>| fun next_expr_env ->
        union_envs [async_expr1_env; async_expr2_env; next_expr_env]

(* top level expression to return - we discard the value used in recursive subcomputation *)
let type_async_expr class_defns trait_defns expr =
  Result.ignore_m (type_async_expr_helper class_defns trait_defns expr)

let type_function_async_body_exprs class_defns trait_defns function_defns =
  Result.all_unit
    (List.map
       ~f:(fun (TFunction (_, _, _, body_expr)) ->
         type_async_expr class_defns trait_defns body_expr)
       function_defns)

let type_class_async_method_body_exprs class_defns trait_defns
    (TClass (_, _, _, method_defns)) =
  type_function_async_body_exprs class_defns trait_defns method_defns

let type_classes_async_method_body_exprs class_defns trait_defns =
  Result.all_unit
    (List.map ~f:(type_class_async_method_body_exprs class_defns trait_defns) class_defns)

let type_program_async_exprs (Prog (class_defns, trait_defns, function_defns, expr)) =
  type_classes_async_method_body_exprs class_defns trait_defns
  >>= fun () ->
  type_function_async_body_exprs class_defns trait_defns function_defns
  >>= fun () -> type_async_expr class_defns trait_defns expr
