open Data_race_checker_ast
open Data_race_checker_env
open Core

let rec type_finish_async_expr expr =
  let remove_thread_cap cap = cap.thread <- false in
  let remove_linear_cap cap = cap.linear <- false in
  match expr with
  | FinishAsync (_, _, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      (* remove thread capabilities from free vars in async exprs (since they've been
         created on current_thread but used in a different thread) *)
      List.iter
        ~f:(fun (AsyncExpr (free_vars, expr)) ->
          List.iter
            ~f:(fun free_var ->
              update_var_capabilities_block_expr free_var remove_thread_cap expr ;
              if elem_in_list free_var curr_thread_free_vars then
                update_var_capabilities_block_expr free_var remove_thread_cap
                  curr_thread_expr)
            free_vars)
        async_exprs ;
      (* remove linear capabilities from free vars if they've been accessed in multiple
         threads *)
      let all_thread_free_vars =
        curr_thread_free_vars
        @ List.concat_map ~f:(fun (AsyncExpr (free_vars, _)) -> free_vars) async_exprs
      in
      let non_linear_vars =
        List.find_all_dups
          ~compare:(fun a b -> if a = b then 0 else 1)
          all_thread_free_vars in
      List.iter
        ~f:(fun dup_free_var ->
          List.iter
            ~f:(fun (AsyncExpr (async_free_vars, async_expr)) ->
              if elem_in_list dup_free_var async_free_vars then
                update_var_capabilities_block_expr dup_free_var remove_linear_cap
                  async_expr)
            async_exprs ;
          if elem_in_list dup_free_var curr_thread_free_vars then
            update_var_capabilities_block_expr dup_free_var remove_linear_cap
              curr_thread_expr)
        non_linear_vars ;
      (* Recursive calls on sub expressions *)
      List.iter
        ~f:(fun (AsyncExpr (_, async_expr)) -> type_finish_async_block_expr async_expr)
        async_exprs ;
      type_finish_async_block_expr curr_thread_expr
  (* Rest of the cases are just recursive calls *)
  | Integer _ | Boolean _ | Identifier _ -> ()
  | BlockExpr (_, block_expr) -> type_finish_async_block_expr block_expr
  | Constructor (_, _, _, constructor_args) ->
      List.iter
        ~f:(fun (ConstructorArg (_, _, expr)) -> type_finish_async_expr expr)
        constructor_args
  | Let (_, _, _, bound_expr) -> type_finish_async_expr bound_expr
  | Assign (_, _, _, assigned_expr) -> type_finish_async_expr assigned_expr
  | Consume (_, _) -> ()
  | MethodApp (_, _, _, _, _, args) -> List.iter ~f:type_finish_async_expr args
  | FunctionApp (_, _, _, args) -> List.iter ~f:type_finish_async_expr args
  | Printf (_, _, args) -> List.iter ~f:type_finish_async_expr args
  | If (_, _, cond_expr, then_expr, else_expr) ->
      type_finish_async_expr cond_expr ;
      type_finish_async_block_expr then_expr ;
      type_finish_async_block_expr else_expr
  | While (_, cond_expr, loop_expr) ->
      type_finish_async_expr cond_expr ;
      type_finish_async_block_expr loop_expr
  | BinOp (_, _, _, expr1, expr2) ->
      type_finish_async_expr expr1 ; type_finish_async_expr expr2
  | UnOp (_, _, _, expr) -> type_finish_async_expr expr

and type_finish_async_block_expr (Block (_, _, exprs)) =
  List.iter ~f:type_finish_async_expr exprs
