open Core
open Result
open Ast_types
open Typed_ast
open Type_env

let remove_bound_var bound_var_name env =
  List.filter ~f:(fun (var_name, _) -> not (var_name = bound_var_name)) env

let union_envs env1 env2 =
  List.dedup_and_sort
    ~compare:(fun (name_1, _) (name_2, _) -> if name_1 = name_2 then 0 else 1)
    (env1 @ env2)

let has_read_or_no_cap type_expr class_defns loc =
  match get_type_capability type_expr class_defns loc with
  | Ok Read | Error _ -> true
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
      | Ok Thread -> true
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
   program. Bound variables in async exprs are not in the scope of the other async expr
   so aren't seen.

   So 1) amounts to checking that fv(expr1) intersect fv(expr2) = ø and 2) amounts to
   checking no free vars in expr2 have type "thread". (Note that any thread variables
   created in second thread are bound, so free "thread" vars must have been from another
   thread.) *)

(* Helper function used to return results of subcomputations when recursively type-check
   expression (top level function returns unit) *)
let rec type_async_expr_helper class_defns trait_defns expr =
  let type_async_expr_with_defns = type_async_expr_helper class_defns trait_defns in
  match expr with
  | Integer (_, _)                                            -> Ok []
  | Variable (_, var_type, var_name)                          -> Ok [(var_name, var_type)]
  | Lambda (_, _, bound_var_name, _, body_expr)               ->
      type_async_expr_with_defns body_expr
      >>| fun body_env -> remove_bound_var bound_var_name body_env
  | App (_, _, func_expr, arg_expr)                           ->
      type_async_expr_with_defns func_expr
      >>= fun func_expr_env ->
      type_async_expr_with_defns arg_expr
      >>| fun arg_expr_env -> union_envs func_expr_env arg_expr_env
  | Seq (_, _, exprs)                                         ->
      Result.all (List.map ~f:type_async_expr_with_defns exprs)
      >>| (* Flatten and take union of all envs *)
          List.fold ~init:[] ~f:(fun acc expr_env -> union_envs acc expr_env)
  | Let (_, _, bound_var_name, subbed_expr, body_expr)        ->
      type_async_expr_with_defns subbed_expr
      >>= fun subbed_expr_env ->
      type_async_expr_with_defns body_expr
      >>| fun body_expr_env ->
      union_envs subbed_expr_env (remove_bound_var bound_var_name body_expr_env)
  | ObjField (_, _, var_name, var_type, _)                    -> Ok [(var_name, var_type)]
  | Assign (_, _, var_name, var_type, _, assigned_expr)       ->
      type_async_expr_with_defns assigned_expr
      >>| fun assign_expr_env -> union_envs assign_expr_env [(var_name, var_type)]
  | Constructor (_, _, _, constructor_args)                   ->
      Result.all
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) -> type_async_expr_with_defns expr)
           constructor_args)
      >>| (* Flatten and take union of all envs *)
          List.fold ~init:[] ~f:(fun acc expr_env -> union_envs acc expr_env)
  | Consume (_, _, expr)                                      -> type_async_expr_with_defns
                                                                   expr
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
                "%s Potential data race: thread-local variable accessed from other \
                 thread.@."
                (string_of_loc loc)))
      else
        (* We're good, so check sub-exprs *)
        type_async_expr_with_defns next_expr
        >>| fun next_expr_env ->
        union_envs (union_envs async_expr1_env async_expr2_env) next_expr_env

(* top level expression to return - we discard the value used in recursive subcomputation *)
let type_async_expr class_defns trait_defns expr =
  Result.ignore (type_async_expr_helper class_defns trait_defns expr)
