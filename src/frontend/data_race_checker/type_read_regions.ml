open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types

let remove_read_regions id =
  let filtered_regions =
    List.filter
      ~f:(fun (TRegion (cap, _)) -> not (cap = Read))
      (get_identifier_regions id) in
  set_identifier_regions id filtered_regions

let rec type_read_regions_expr expr =
  match expr with
  | Integer _ | Boolean _ -> expr
  | Identifier (loc, id) -> Identifier (loc, id)
  | BlockExpr (loc, block_expr) -> BlockExpr (loc, type_read_regions_block_expr block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg (type_expr, field_name, type_read_regions_expr expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let (loc, type_expr, var_name, type_read_regions_expr bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign (loc, type_expr, remove_read_regions id, type_read_regions_expr assigned_expr)
  | Consume (loc, id) -> Consume (loc, remove_read_regions id)
  | MethodApp (loc, type_expr, obj_name, obj_type, method_name, args) ->
      MethodApp
        ( loc
        , type_expr
        , obj_name
        , obj_type
        , method_name
        , List.map ~f:type_read_regions_expr args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp (loc, return_type, func_name, List.map ~f:type_read_regions_expr args)
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:type_read_regions_expr args)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, expr)) ->
              AsyncExpr (free_vars, type_read_regions_block_expr expr))
            async_exprs
        , curr_thread_free_vars
        , type_read_regions_block_expr curr_thread_expr )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , type_read_regions_expr cond_expr
        , type_read_regions_block_expr then_expr
        , type_read_regions_block_expr else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While (loc, type_read_regions_expr cond_expr, type_read_regions_block_expr loop_expr)
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        (loc, type_expr, binop, type_read_regions_expr expr1, type_read_regions_expr expr2)
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, type_read_regions_expr expr)

and type_read_regions_block_expr (Block (loc, type_block_expr, exprs)) =
  let updated_exprs = List.map ~f:type_read_regions_expr exprs in
  Block (loc, type_block_expr, updated_exprs)
