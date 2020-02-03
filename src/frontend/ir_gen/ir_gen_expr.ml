open Ast.Ast_types
open Core
open Ir_gen_operators
open Ir_gen_env

let ir_gen_identifier class_defns id =
  let open Result in
  match id with
  | Desugaring.Desugared_ast.Variable (_, var_name) ->
      Ok (Frontend_ir.Variable (Var_name.to_string var_name))
  | Desugaring.Desugared_ast.ObjField (obj_type, obj_name, _, field_name) ->
      ir_gen_field_index field_name obj_type class_defns
      >>| fun ir_field_index ->
      Frontend_ir.ObjField (Var_name.to_string obj_name, ir_field_index)

let rec ir_gen_expr class_defns expr =
  let open Result in
  match expr with
  | Desugaring.Desugared_ast.Integer (_, i) -> Ok (Frontend_ir.Integer i)
  | Desugaring.Desugared_ast.Boolean (_, b) -> Ok (Frontend_ir.Boolean b)
  | Desugaring.Desugared_ast.Identifier (_, id) ->
      ir_gen_identifier class_defns id >>| fun ir_id -> Frontend_ir.Identifier ir_id
  | Desugaring.Desugared_ast.BlockExpr (_, block_expr) ->
      ir_gen_block_expr class_defns block_expr
      >>| fun ir_block_expr -> Frontend_ir.Block ir_block_expr
  | Desugaring.Desugared_ast.Constructor (_, _, class_name, constructor_args) ->
      Result.all
        (List.map ~f:(ir_gen_constructor_arg class_name class_defns) constructor_args)
      >>| fun ir_constructor_args ->
      Frontend_ir.Constructor (Class_name.to_string class_name, ir_constructor_args)
  | Desugaring.Desugared_ast.Let (_, _, var_name, bound_expr) ->
      ir_gen_expr class_defns bound_expr
      >>| fun ir_bound_expr -> Frontend_ir.Let (Var_name.to_string var_name, ir_bound_expr)
  | Desugaring.Desugared_ast.Assign (_, _, id, assigned_expr) ->
      ir_gen_identifier class_defns id
      >>= fun ir_id ->
      ir_gen_expr class_defns assigned_expr
      >>| fun ir_assigned_expr -> Frontend_ir.Assign (ir_id, ir_assigned_expr)
  | Desugaring.Desugared_ast.Consume (_, id) ->
      ir_gen_identifier class_defns id >>| fun ir_id -> Frontend_ir.Consume ir_id
  | Desugaring.Desugared_ast.MethodApp (_, _, this, obj_type, method_name, args) ->
      ir_gen_method_name method_name obj_type
      >>= fun ir_method_name ->
      Result.all (List.map ~f:(ir_gen_expr class_defns) args)
      >>| fun ir_args ->
      Frontend_ir.FunctionApp
        ( ir_method_name
        , Frontend_ir.Identifier (Frontend_ir.Variable (Var_name.to_string this))
          :: ir_args )
  | Desugaring.Desugared_ast.FunctionApp (_, _, func_name, args) ->
      Result.all (List.map ~f:(ir_gen_expr class_defns) args)
      >>| fun ir_args ->
      Frontend_ir.FunctionApp (Function_name.to_string func_name, ir_args)
  | Desugaring.Desugared_ast.Printf (_, format_str, args) ->
      Result.all (List.map ~f:(ir_gen_expr class_defns) args)
      >>| fun ir_args -> Frontend_ir.Printf (format_str, ir_args)
  | Desugaring.Desugared_ast.FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      Result.all (List.map ~f:(ir_gen_async_expr class_defns) async_exprs)
      >>= fun ir_async_exprs ->
      ir_gen_block_expr class_defns curr_thread_expr
      >>| fun ir_curr_thread_expr ->
      Frontend_ir.FinishAsync (ir_async_exprs, ir_curr_thread_expr)
  | Desugaring.Desugared_ast.If (_, _, cond_expr, then_expr, else_expr) ->
      ir_gen_expr class_defns cond_expr
      >>= fun ir_cond_expr ->
      ir_gen_block_expr class_defns then_expr
      >>= fun ir_then_expr ->
      ir_gen_block_expr class_defns else_expr
      >>| fun ir_else_expr -> Frontend_ir.IfElse (ir_cond_expr, ir_then_expr, ir_else_expr)
  | Desugaring.Desugared_ast.While (_, cond_expr, loop_expr) ->
      ir_gen_expr class_defns cond_expr
      >>= fun ir_cond_expr ->
      ir_gen_block_expr class_defns loop_expr
      >>| fun ir_loop_expr -> Frontend_ir.WhileLoop (ir_cond_expr, ir_loop_expr)
  | Desugaring.Desugared_ast.BinOp (_, _, bin_op, expr1, expr2) ->
      ir_gen_expr class_defns expr1
      >>= fun ir_expr1 ->
      ir_gen_expr class_defns expr2
      >>| fun ir_expr2 -> Frontend_ir.BinOp (ir_gen_bin_op bin_op, ir_expr1, ir_expr2)
  | Desugaring.Desugared_ast.UnOp (_, _, un_op, expr) ->
      ir_gen_expr class_defns expr
      >>| fun ir_expr -> Frontend_ir.UnOp (ir_gen_un_op un_op, ir_expr)

and ir_gen_block_expr class_defns (Desugaring.Desugared_ast.Block (_, _, exprs)) =
  Result.all (List.map ~f:(ir_gen_expr class_defns) exprs)

and ir_gen_async_expr class_defns (Desugaring.Desugared_ast.AsyncExpr (free_vars, expr)) =
  let open Result in
  List.map ~f:Var_name.to_string free_vars
  |> fun string_free_vars ->
  ir_gen_block_expr class_defns expr
  >>| fun ir_exprs -> Frontend_ir.AsyncExpr (string_free_vars, ir_exprs)

and ir_gen_constructor_arg class_name class_defns
    (Desugaring.Desugared_ast.ConstructorArg (_, field_name, expr)) =
  let open Result in
  ir_gen_field_index field_name (TEClass class_name) class_defns
  >>= fun ir_field_index ->
  ir_gen_expr class_defns expr
  >>| fun ir_expr -> Frontend_ir.ConstructorArg (ir_field_index, ir_expr)
