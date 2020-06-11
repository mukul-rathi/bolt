open Core
open Desugaring.Desugared_ast
open Data_race_checker_env

let update_capabilities_if_match names_to_match capability_filter_fn name capabilities =
  if elem_in_list name names_to_match then
    List.filter ~f:(capability_filter_fn capabilities) capabilities
  else capabilities

let update_matching_identifier_caps names_to_match capability_filter_fn id =
  match id with
  | Variable (var_type, name, capabilities, maybeBorrowed) ->
      Variable
        ( var_type
        , name
        , update_capabilities_if_match names_to_match capability_filter_fn name
            capabilities
        , maybeBorrowed )
  | ObjField (obj_class, obj_name, field_type, field_name, capabilities, maybeBorrowed) ->
      ObjField
        ( obj_class
        , obj_name
        , field_type
        , field_name
        , update_capabilities_if_match names_to_match capability_filter_fn obj_name
            capabilities
        , maybeBorrowed )

let rec update_matching_identifier_caps_expr names_to_match capability_filter_fn expr =
  let update_matching_identifier_caps_expr_rec =
    update_matching_identifier_caps_expr names_to_match capability_filter_fn in
  let update_matching_identifier_caps_block_expr_rec =
    update_matching_identifier_caps_block_expr names_to_match capability_filter_fn in
  let update_var_modes_identifier_rec =
    update_matching_identifier_caps names_to_match capability_filter_fn in
  match expr with
  | Integer _ | Boolean _ -> expr
  | Identifier (loc, id) -> Identifier (loc, update_var_modes_identifier_rec id)
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, update_matching_identifier_caps_block_expr_rec block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg
              (type_expr, field_name, update_matching_identifier_caps_expr_rec expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_args)
  | Let (loc, type_expr, names_to_match, bound_expr) ->
      Let
        ( loc
        , type_expr
        , names_to_match
        , update_matching_identifier_caps_expr_rec bound_expr )
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign
        ( loc
        , type_expr
        , update_var_modes_identifier_rec id
        , update_matching_identifier_caps_expr_rec assigned_expr )
  | Consume (loc, id) -> Consume (loc, update_var_modes_identifier_rec id)
  | MethodApp (loc, type_expr, obj_name, obj_capabilities, obj_class, method_name, args)
    ->
      MethodApp
        ( loc
        , type_expr
        , obj_name
        , update_capabilities_if_match names_to_match capability_filter_fn obj_name
            obj_capabilities
        , obj_class
        , method_name
        , List.map ~f:update_matching_identifier_caps_expr_rec args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp
        ( loc
        , return_type
        , func_name
        , List.map ~f:update_matching_identifier_caps_expr_rec args )
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:update_matching_identifier_caps_expr_rec args)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, expr)) ->
              AsyncExpr (free_vars, update_matching_identifier_caps_block_expr_rec expr))
            async_exprs
        , curr_thread_free_vars
        , update_matching_identifier_caps_block_expr_rec curr_thread_expr )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , update_matching_identifier_caps_expr_rec cond_expr
        , update_matching_identifier_caps_block_expr_rec then_expr
        , update_matching_identifier_caps_block_expr_rec else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        ( loc
        , update_matching_identifier_caps_expr_rec cond_expr
        , update_matching_identifier_caps_block_expr_rec loop_expr )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , update_matching_identifier_caps_expr_rec expr1
        , update_matching_identifier_caps_expr_rec expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, update_matching_identifier_caps_expr_rec expr)

and update_matching_identifier_caps_block_expr names_to_match capability_filter_fn
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs =
    List.map
      ~f:(update_matching_identifier_caps_expr names_to_match capability_filter_fn)
      exprs in
  Block (loc, type_block_expr, updated_exprs)
