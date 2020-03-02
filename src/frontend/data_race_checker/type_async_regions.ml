open Desugaring.Desugared_ast
open Data_race_checker_env
open Core
open Update_identifier_regions
open Ast.Ast_types

(* filter regions that are themselves thread-local or subord or share state that are not
   safe() with thread or subord regions *)
let filter_regions_with_thread_or_subord_state class_name class_defns all_regions
    curr_region =
  let thread_or_subord_regions =
    List.filter
      ~f:(fun region ->
        region_fields_have_capability region class_name ThreadLocal class_defns
        || region_fields_have_capability region class_name Subordinate class_defns)
      all_regions in
  (* check we can concurrently access this region with the thread or subord regions, i.e
     all overlapping state is safe() *)
  List.for_all
    ~f:(fun region ->
      can_concurrently_access_regions class_name class_defns region curr_region)
    thread_or_subord_regions

let remove_thread_regions_from_async_expr class_defns
    (AsyncExpr (free_var_types_and_regions, async_expr)) =
  (* update async expression if the free variable contains thread-local state - remove
     those regions *)
  let updated_async_expr =
    List.fold ~init:async_expr
      ~f:(fun acc_expr (obj_name, obj_class, _) ->
        let obj_is_subord_of_thread =
          class_has_capability obj_class ThreadLocal class_defns
          || class_has_capability obj_class Subordinate class_defns in
        if obj_is_subord_of_thread then
          update_identifier_regions_block_expr obj_name
            (filter_regions_with_thread_or_subord_state obj_class class_defns)
            acc_expr
        else acc_expr)
      free_var_types_and_regions in
  AsyncExpr (free_var_types_and_regions, updated_async_expr)

let rec type_async_regions_expr class_defns expr =
  match expr with
  | FinishAsync
      (loc, type_expr, async_exprs, curr_thread_free_vars_and_types, curr_thread_expr) ->
      let typed_thread_async_exprs =
        List.map
          ~f:(fun async_expr ->
            remove_thread_regions_from_async_expr class_defns async_expr)
          async_exprs in
      (* Recursive calls on sub expressions *)
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars_types_and_regions, async_expr)) ->
              AsyncExpr
                ( free_vars_types_and_regions
                , type_async_regions_block_expr class_defns async_expr ))
            typed_thread_async_exprs
        , curr_thread_free_vars_and_types
        , type_async_regions_block_expr class_defns curr_thread_expr )
  (* Rest of the cases are just recursive calls *)
  | Integer _ | Boolean _ | Identifier _ -> expr
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, type_async_regions_block_expr class_defns block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_constructor_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg
              (type_expr, field_name, type_async_regions_expr class_defns expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_constructor_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let (loc, type_expr, var_name, type_async_regions_expr class_defns bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign (loc, type_expr, id, type_async_regions_expr class_defns assigned_expr)
  | Consume _ -> expr
  | MethodApp (loc, return_type, obj_name, obj_type, method_name, args) ->
      MethodApp
        ( loc
        , return_type
        , obj_name
        , obj_type
        , method_name
        , List.map ~f:(type_async_regions_expr class_defns) args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp
        ( loc
        , return_type
        , func_name
        , List.map ~f:(type_async_regions_expr class_defns) args )
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:(type_async_regions_expr class_defns) args)
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , type_async_regions_expr class_defns cond_expr
        , type_async_regions_block_expr class_defns then_expr
        , type_async_regions_block_expr class_defns else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        ( loc
        , type_async_regions_expr class_defns cond_expr
        , type_async_regions_block_expr class_defns loop_expr )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , type_async_regions_expr class_defns expr1
        , type_async_regions_expr class_defns expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, type_async_regions_expr class_defns expr)

and type_async_regions_block_expr class_defns (Block (loc, type_expr, exprs)) =
  Block (loc, type_expr, List.map ~f:(type_async_regions_expr class_defns) exprs)
