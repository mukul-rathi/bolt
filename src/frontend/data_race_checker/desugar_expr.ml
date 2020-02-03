open Core
open Free_vars_expr

let desugar_identifier = function
  | Typing.Typed_ast.Variable (var_type, var_name) ->
      Data_race_checker_ast.Variable (var_type, var_name)
  | Typing.Typed_ast.ObjField (obj_type, obj_name, field_type, field_name) ->
      Data_race_checker_ast.ObjField (obj_type, obj_name, field_type, field_name)

let rec desugar_expr expr =
  (* Helper function since desugar_expr recursive call returns a list not a single expr *)
  let open Result in
  match expr with
  | Typing.Typed_ast.Integer (loc, i) -> Ok (Data_race_checker_ast.Integer (loc, i))
  | Typing.Typed_ast.Boolean (loc, b) -> Ok (Data_race_checker_ast.Boolean (loc, b))
  | Typing.Typed_ast.Identifier (loc, id) ->
      Ok (Data_race_checker_ast.Identifier (loc, desugar_identifier id))
  | Typing.Typed_ast.BlockExpr (loc, block_expr) ->
      desugar_block_expr block_expr
      >>| fun desugared_block_expr ->
      Data_race_checker_ast.BlockExpr (loc, desugared_block_expr)
  | Typing.Typed_ast.Constructor (loc, type_expr, class_name, constructor_args) ->
      (* Each constructor arg has a separate environment, so we don't accumulate var maps *)
      Result.all
        (List.map
           ~f:(fun (Typing.Typed_ast.ConstructorArg (type_expr, field_name, expr)) ->
             desugar_expr expr
             >>| fun desugared_expr ->
             Data_race_checker_ast.ConstructorArg (type_expr, field_name, desugared_expr))
           constructor_args)
      >>| fun desugared_constructor_args ->
      Data_race_checker_ast.Constructor
        (loc, type_expr, class_name, desugared_constructor_args)
  | Typing.Typed_ast.Let (loc, type_expr, var_name, bound_expr) ->
      desugar_expr bound_expr
      >>| fun desugared_bound_expr ->
      Data_race_checker_ast.Let (loc, type_expr, var_name, desugared_bound_expr)
  | Typing.Typed_ast.Assign (loc, type_expr, id, assigned_expr) ->
      desugar_expr assigned_expr
      >>| fun desugared_assigned_expr ->
      Data_race_checker_ast.Assign
        (loc, type_expr, desugar_identifier id, desugared_assigned_expr)
  | Typing.Typed_ast.Consume (loc, id) ->
      Ok (Data_race_checker_ast.Consume (loc, desugar_identifier id))
  | Typing.Typed_ast.MethodApp (loc, type_expr, var_name, obj_type, method_name, args) ->
      Result.all (List.map ~f:desugar_expr args)
      >>| fun desugared_args ->
      Data_race_checker_ast.MethodApp
        (loc, type_expr, var_name, obj_type, method_name, desugared_args)
  | Typing.Typed_ast.FunctionApp (loc, type_expr, func_name, args) ->
      Result.all (List.map ~f:desugar_expr args)
      >>| fun desugared_args ->
      Data_race_checker_ast.FunctionApp (loc, type_expr, func_name, desugared_args)
  | Typing.Typed_ast.Printf (loc, format_str, args) ->
      Result.all (List.map ~f:desugar_expr args)
      >>| fun desugared_args ->
      Data_race_checker_ast.Printf (loc, format_str, desugared_args)
  | Typing.Typed_ast.FinishAsync (loc, type_expr, async_exprs, curr_thread_expr) ->
      Result.all (List.map ~f:desugar_async_expr async_exprs)
      >>= fun desugared_async_exprs ->
      desugar_block_expr curr_thread_expr
      >>| fun desugared_curr_thread_expr ->
      Data_race_checker_ast.FinishAsync
        (loc, type_expr, desugared_async_exprs, desugared_curr_thread_expr)
  | Typing.Typed_ast.If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      desugar_expr cond_expr
      >>= fun desugared_cond_expr ->
      desugar_block_expr then_expr
      >>= fun desugared_then_expr ->
      desugar_block_expr else_expr
      >>| fun desugared_else_expr ->
      Data_race_checker_ast.If
        (loc, type_expr, desugared_cond_expr, desugared_then_expr, desugared_else_expr)
  | Typing.Typed_ast.While (loc, cond_expr, loop_expr) ->
      desugar_expr cond_expr
      >>= fun desugared_cond_expr ->
      desugar_block_expr loop_expr
      >>| fun desugared_loop_expr ->
      Data_race_checker_ast.While (loc, desugared_cond_expr, desugared_loop_expr)
  | Typing.Typed_ast.BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      desugar_expr expr1
      >>= fun desugared_expr1 ->
      desugar_expr expr2
      >>| fun desugared_expr2 ->
      Data_race_checker_ast.BinOp
        (loc, type_expr, bin_op, desugared_expr1, desugared_expr2)
  | Typing.Typed_ast.UnOp (loc, type_expr, un_op, expr) ->
      desugar_expr expr
      >>| fun desugared_expr ->
      Data_race_checker_ast.UnOp (loc, type_expr, un_op, desugared_expr)

and desugar_block_expr (Typing.Typed_ast.Block (loc, type_expr, exprs)) =
  let open Result in
  Result.all (List.map ~f:desugar_expr exprs)
  >>| fun desugared_exprs -> Data_race_checker_ast.Block (loc, type_expr, desugared_exprs)

and desugar_async_expr (Typing.Typed_ast.AsyncExpr async_block_expr) =
  let open Result in
  desugar_block_expr async_block_expr
  >>| fun desugared_async_block_expr ->
  let free_vars_expr = free_vars_block_expr desugared_async_block_expr in
  Data_race_checker_ast.AsyncExpr (free_vars_expr, desugared_async_block_expr)
