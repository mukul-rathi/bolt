open Ast.Ast_types
open Core
open Ir_gen_operators
open Ir_gen_env
open Desugaring

let ir_gen_identifier class_defns id =
  let should_lock_id capabilities =
    let locked_capabilities =
      List.filter ~f:(fun (TCapability (mode, _)) -> mode = Locked) capabilities in
    List.length locked_capabilities > 0 in
  match id with
  | Desugared_ast.Variable (_, var_name, capabilities, _) ->
      (Frontend_ir.Variable (Var_name.to_string var_name), should_lock_id capabilities)
  | Desugared_ast.ObjField (class_name, obj_name, _, field_name, capabilities, _) ->
      ir_gen_field_index field_name class_name class_defns
      |> fun ir_field_index ->
      ( Frontend_ir.ObjField (Var_name.to_string obj_name, ir_field_index)
      , should_lock_id capabilities )

let rec ir_gen_expr class_defns expr =
  match expr with
  | Desugared_ast.Integer (_, i) -> Frontend_ir.Integer i
  | Desugared_ast.Boolean (_, b) -> Frontend_ir.Boolean b
  | Desugared_ast.Identifier (_, id) ->
      ir_gen_identifier class_defns id
      |> fun (ir_id, should_lock) ->
      let lock_held = if should_lock then Some Frontend_ir.Reader else None in
      Frontend_ir.Identifier (ir_id, lock_held)
  | Desugared_ast.BlockExpr (_, block_expr) ->
      ir_gen_block_expr class_defns block_expr
      |> fun ir_block_expr -> Frontend_ir.Block ir_block_expr
  | Desugared_ast.Constructor (_, _, class_name, constructor_args) ->
      List.map ~f:(ir_gen_constructor_arg class_name class_defns) constructor_args
      |> fun ir_constructor_args ->
      Frontend_ir.Constructor (Class_name.to_string class_name, ir_constructor_args)
  | Desugared_ast.Let (_, _, var_name, bound_expr) ->
      ir_gen_expr class_defns bound_expr
      |> fun ir_bound_expr -> Frontend_ir.Let (Var_name.to_string var_name, ir_bound_expr)
  | Desugared_ast.Assign (_, _, id, assigned_expr) ->
      ir_gen_identifier class_defns id
      |> fun (ir_id, should_lock) ->
      ir_gen_expr class_defns assigned_expr
      |> fun ir_assigned_expr ->
      let lock_held = if should_lock then Some Frontend_ir.Writer else None in
      Frontend_ir.Assign (ir_id, ir_assigned_expr, lock_held)
  | Desugared_ast.Consume (_, id) ->
      ir_gen_identifier class_defns id
      |> fun (ir_id, should_lock) ->
      let lock_held = if should_lock then Some Frontend_ir.Writer else None in
      Frontend_ir.Consume (ir_id, lock_held)
  | Desugared_ast.MethodApp (_, _, obj_name, _, obj_class, method_name, args) ->
      ir_gen_vtable_method_index method_name obj_class class_defns
      |> fun ir_method_index ->
      List.map ~f:(ir_gen_expr class_defns) args
      |> fun ir_args ->
      Frontend_ir.MethodApp
        ( Var_name.to_string obj_name
        , name_mangle_method_name method_name obj_class
        , ir_method_index
        , ir_args )
  | Desugared_ast.FunctionApp (_, _, func_name, args) ->
      List.map ~f:(ir_gen_expr class_defns) args
      |> fun ir_args ->
      Frontend_ir.FunctionApp (Function_name.to_string func_name, ir_args)
  | Desugared_ast.Printf (_, format_str, args) ->
      List.map ~f:(ir_gen_expr class_defns) args
      |> fun ir_args -> Frontend_ir.Printf (format_str, ir_args)
  | Desugared_ast.FinishAsync (_, _, async_exprs, _, curr_thread_expr) ->
      List.map ~f:(ir_gen_async_expr class_defns) async_exprs
      |> fun ir_async_exprs ->
      ir_gen_block_expr class_defns curr_thread_expr
      |> fun ir_curr_thread_expr ->
      Frontend_ir.FinishAsync (ir_async_exprs, ir_curr_thread_expr)
  | Desugared_ast.If (_, _, cond_expr, then_expr, else_expr) ->
      ir_gen_expr class_defns cond_expr
      |> fun ir_cond_expr ->
      ir_gen_block_expr class_defns then_expr
      |> fun ir_then_expr ->
      ir_gen_block_expr class_defns else_expr
      |> fun ir_else_expr -> Frontend_ir.IfElse (ir_cond_expr, ir_then_expr, ir_else_expr)
  | Desugared_ast.While (_, cond_expr, loop_expr) ->
      ir_gen_expr class_defns cond_expr
      |> fun ir_cond_expr ->
      ir_gen_block_expr class_defns loop_expr
      |> fun ir_loop_expr -> Frontend_ir.WhileLoop (ir_cond_expr, ir_loop_expr)
  | Desugared_ast.BinOp (_, _, bin_op, expr1, expr2) ->
      ir_gen_expr class_defns expr1
      |> fun ir_expr1 ->
      ir_gen_expr class_defns expr2
      |> fun ir_expr2 -> Frontend_ir.BinOp (ir_gen_bin_op bin_op, ir_expr1, ir_expr2)
  | Desugared_ast.UnOp (_, _, un_op, expr) ->
      ir_gen_expr class_defns expr
      |> fun ir_expr -> Frontend_ir.UnOp (ir_gen_un_op un_op, ir_expr)

and ir_gen_block_expr class_defns (Desugared_ast.Block (_, _, exprs)) =
  List.map ~f:(ir_gen_expr class_defns) exprs

and ir_gen_async_expr class_defns (Desugared_ast.AsyncExpr (free_vars, expr)) =
  List.map ~f:(fun (var_name, _, _) -> Var_name.to_string var_name) free_vars
  |> fun string_free_vars ->
  ir_gen_block_expr class_defns expr
  |> fun ir_exprs -> Frontend_ir.AsyncExpr (string_free_vars, ir_exprs)

and ir_gen_constructor_arg class_name class_defns
    (Desugared_ast.ConstructorArg (_, field_name, expr)) =
  ir_gen_field_index field_name class_name class_defns
  |> fun ir_field_index ->
  ir_gen_expr class_defns expr
  |> fun ir_expr -> Frontend_ir.ConstructorArg (ir_field_index, ir_expr)
