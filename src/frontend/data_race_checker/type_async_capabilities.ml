open Desugaring.Desugared_ast
open Data_race_checker_env
open Core
open Update_identifier_capabilities
open Type_concurrent_capability_access
open Ast.Ast_types

(* filter capabilities that are themselves thread-local or subord or share state that are
   not safe() with thread or subord capabilities *)
let filter_capabilities_with_thread_or_subord_state class_name class_defns
    all_capabilities curr_capability =
  let thread_or_subord_capabilities =
    List.filter
      ~f:(fun capability ->
        capability_fields_have_mode capability class_name ThreadLocal class_defns
        || capability_fields_have_mode capability class_name Subordinate class_defns)
      all_capabilities in
  (* check we can concurrently access this capability with the thread or subord
     capabilities, i.e all overlapping state is safe() *)
  List.for_all
    ~f:(fun capability ->
      can_concurrently_access_capabilities class_name class_defns capability
        curr_capability)
    thread_or_subord_capabilities

let remove_thread_subord_caps_from_async_expr class_defns
    (AsyncExpr (free_var_types_and_capabilities, async_expr)) =
  (* update async expression if the free variable contains thread-local or subord state -
     remove those capabilities from all aliases *)
  let updated_async_expr =
    List.fold ~init:async_expr
      ~f:(fun acc_async_expr (obj_name, obj_class, _) ->
        let obj_is_subord_of_thread =
          class_has_mode obj_class ThreadLocal class_defns
          || class_has_mode obj_class Subordinate class_defns in
        if obj_is_subord_of_thread then
          let aliases_to_remove_subord_thread_caps =
            find_aliases_in_block_expr ~should_match_fields:true obj_name acc_async_expr
            (* match fields since if x is not thread-local, x.f isn't *) in
          update_matching_identifier_caps_block_expr
            (obj_name :: aliases_to_remove_subord_thread_caps)
            (filter_capabilities_with_thread_or_subord_state obj_class class_defns)
            acc_async_expr
        else acc_async_expr)
      free_var_types_and_capabilities in
  AsyncExpr (free_var_types_and_capabilities, updated_async_expr)

let rec type_async_capabilities_expr class_defns expr =
  match expr with
  | FinishAsync
      (loc, type_expr, async_exprs, curr_thread_free_vars_and_types, curr_thread_expr) ->
      let typed_async_exprs =
        List.map
          ~f:(fun async_expr ->
            remove_thread_subord_caps_from_async_expr class_defns async_expr)
          async_exprs in
      (* Recursive calls on sub expressions *)
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars_types_and_capabilities, async_expr)) ->
              AsyncExpr
                ( free_vars_types_and_capabilities
                , type_async_capabilities_block_expr class_defns async_expr ))
            typed_async_exprs
        , curr_thread_free_vars_and_types
        , type_async_capabilities_block_expr class_defns curr_thread_expr )
  (* Rest of the cases are just recursive calls *)
  | Integer _ | Boolean _ | Identifier _ -> expr
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, type_async_capabilities_block_expr class_defns block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_constructor_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg
              (type_expr, field_name, type_async_capabilities_expr class_defns expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_constructor_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let (loc, type_expr, var_name, type_async_capabilities_expr class_defns bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign (loc, type_expr, id, type_async_capabilities_expr class_defns assigned_expr)
  | Consume _ -> expr
  | MethodApp (loc, return_type, obj_name, obj_capabilities, obj_type, method_name, args)
    ->
      MethodApp
        ( loc
        , return_type
        , obj_name
        , obj_capabilities
        , obj_type
        , method_name
        , List.map ~f:(type_async_capabilities_expr class_defns) args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp
        ( loc
        , return_type
        , func_name
        , List.map ~f:(type_async_capabilities_expr class_defns) args )
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:(type_async_capabilities_expr class_defns) args)
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , type_async_capabilities_expr class_defns cond_expr
        , type_async_capabilities_block_expr class_defns then_expr
        , type_async_capabilities_block_expr class_defns else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        ( loc
        , type_async_capabilities_expr class_defns cond_expr
        , type_async_capabilities_block_expr class_defns loop_expr )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , type_async_capabilities_expr class_defns expr1
        , type_async_capabilities_expr class_defns expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, type_async_capabilities_expr class_defns expr)

and type_async_capabilities_block_expr class_defns (Block (loc, type_expr, exprs)) =
  Block (loc, type_expr, List.map ~f:(type_async_capabilities_expr class_defns) exprs)
