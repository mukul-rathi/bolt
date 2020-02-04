open Data_race_checker_ast
open Data_race_checker_env
open Core
open Update_var_capabilities_expr

(* check if this free var is in the current thread expression, if so update the current
   expression with its capability *)
let update_expr_if_vars_present curr_expr all_free_vars curr_expr_free_vars cap_update_fn
    =
  List.fold ~init:curr_expr
    ~f:(fun acc_curr_thread_expr free_var ->
      (* check if this free var is in the current thread expression, if so remove its
         thread capability *)
      if elem_in_list free_var curr_expr_free_vars then
        update_var_capabilities_block_expr free_var cap_update_fn acc_curr_thread_expr
      else acc_curr_thread_expr)
    all_free_vars

let type_thread_finish_async_expr async_exprs curr_thread_free_vars curr_thread_expr =
  (* remove thread capabilities from free vars in async exprs (since they've been created
     on current_thread but used in a different thread) *)
  let remove_thread_cap cap = cap.thread <- false in
  let updated_async_exprs =
    List.map
      ~f:(fun (AsyncExpr (free_vars, expr)) ->
        (* remove thread capabilities from each free variable in the async expression *)
        let updated_expr =
          List.fold ~init:expr
            ~f:(fun acc_expr free_var ->
              update_var_capabilities_block_expr free_var remove_thread_cap acc_expr)
            free_vars in
        AsyncExpr (free_vars, updated_expr))
      async_exprs in
  (* if a variable in the current thread has been accessed in another async thread, remove
     its thread capability *)
  let all_async_expr_free_vars =
    List.concat_map ~f:(fun (AsyncExpr (free_vars, _)) -> free_vars) async_exprs in
  let updated_curr_thread_expr =
    update_expr_if_vars_present curr_thread_expr all_async_expr_free_vars
      curr_thread_free_vars remove_thread_cap in
  (updated_async_exprs, updated_curr_thread_expr)

let type_linear_finish_async_expr async_exprs curr_thread_free_vars curr_thread_expr =
  (* remove linear capabilities from free vars if they've been accessed in multiple
     threads *)
  let remove_linear_cap cap = cap.linear <- false in
  let all_thread_free_vars =
    curr_thread_free_vars
    @ List.concat_map ~f:(fun (AsyncExpr (free_vars, _)) -> free_vars) async_exprs in
  let non_linear_vars =
    List.find_all_dups ~compare:(fun a b -> if a = b then 0 else 1) all_thread_free_vars
  in
  let updated_async_exprs =
    List.map
      ~f:(fun (AsyncExpr (async_free_vars, async_expr)) ->
        let updated_expr =
          update_expr_if_vars_present async_expr non_linear_vars async_free_vars
            remove_linear_cap in
        AsyncExpr (async_free_vars, updated_expr))
      async_exprs in
  let updated_curr_thread_expr =
    update_expr_if_vars_present curr_thread_expr non_linear_vars curr_thread_free_vars
      remove_linear_cap in
  (updated_async_exprs, updated_curr_thread_expr)

let rec type_finish_async_expr expr =
  match expr with
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      let thread_updated_async_exprs, thread_updated_curr_thread_expr =
        type_thread_finish_async_expr async_exprs curr_thread_free_vars curr_thread_expr
      in
      let linear_updated_async_exprs, linear_updated_curr_thread_expr =
        type_linear_finish_async_expr thread_updated_async_exprs curr_thread_free_vars
          thread_updated_curr_thread_expr in
      (* Recursive calls on sub expressions *)
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, async_expr)) ->
              AsyncExpr (free_vars, type_finish_async_block_expr async_expr))
            linear_updated_async_exprs
        , curr_thread_free_vars
        , type_finish_async_block_expr linear_updated_curr_thread_expr )
  (* Rest of the cases are just recursive calls *)
  | Integer _ | Boolean _ | Identifier _ -> expr
  | BlockExpr (loc, block_expr) -> BlockExpr (loc, type_finish_async_block_expr block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_constructor_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg (type_expr, field_name, type_finish_async_expr expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_constructor_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let (loc, type_expr, var_name, type_finish_async_expr bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign (loc, type_expr, id, type_finish_async_expr assigned_expr)
  | Consume _ -> expr
  | MethodApp (loc, return_type, obj_name, obj_type, method_name, args) ->
      MethodApp
        ( loc
        , return_type
        , obj_name
        , obj_type
        , method_name
        , List.map ~f:type_finish_async_expr args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp (loc, return_type, func_name, List.map ~f:type_finish_async_expr args)
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:type_finish_async_expr args)
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , type_finish_async_expr cond_expr
        , type_finish_async_block_expr then_expr
        , type_finish_async_block_expr else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While (loc, type_finish_async_expr cond_expr, type_finish_async_block_expr loop_expr)
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        (loc, type_expr, binop, type_finish_async_expr expr1, type_finish_async_expr expr2)
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, type_finish_async_expr expr)

and type_finish_async_block_expr (Block (loc, type_expr, exprs)) =
  Block (loc, type_expr, List.map ~f:type_finish_async_expr exprs)
