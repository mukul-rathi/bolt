open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types

let remove_subord_capabilities class_defns id =
  let filtered_capabilities class_name =
    List.filter
      ~f:(fun capability ->
        not (capability_fields_have_mode capability class_name Subordinate class_defns))
      (get_identifier_capabilities id) in
  match id with
  | Variable (var_type, var_name, _) -> (
    match var_type with
    | TEClass (var_class, _) ->
        Variable (var_type, var_name, filtered_capabilities var_class)
    | _                      -> id (* nothing to update *) )
  | ObjField (obj_class, obj_name, field_type, field_name, _) ->
      ObjField
        (obj_class, obj_name, field_type, field_name, filtered_capabilities obj_class)

let rec remove_subord_capabilities_expr class_defns expr =
  match expr with
  | Integer _ | Boolean _ -> expr
  | Identifier (loc, id) -> Identifier (loc, remove_subord_capabilities class_defns id)
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, remove_subord_capabilities_block_expr class_defns block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg
              (type_expr, field_name, remove_subord_capabilities_expr class_defns expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let
        (loc, type_expr, var_name, remove_subord_capabilities_expr class_defns bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign
        ( loc
        , type_expr
        , remove_subord_capabilities class_defns id
        , remove_subord_capabilities_expr class_defns assigned_expr )
  | Consume (loc, id) -> Consume (loc, remove_subord_capabilities class_defns id)
  | MethodApp (loc, type_expr, obj_name, obj_type, method_name, args) ->
      MethodApp
        ( loc
        , type_expr
        , obj_name
        , obj_type
        , method_name
        , List.map ~f:(remove_subord_capabilities_expr class_defns) args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp
        ( loc
        , return_type
        , func_name
        , List.map ~f:(remove_subord_capabilities_expr class_defns) args )
  | Printf (loc, format_str, args) ->
      Printf
        (loc, format_str, List.map ~f:(remove_subord_capabilities_expr class_defns) args)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, expr)) ->
              AsyncExpr (free_vars, remove_subord_capabilities_block_expr class_defns expr))
            async_exprs
        , curr_thread_free_vars
        , remove_subord_capabilities_block_expr class_defns curr_thread_expr )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , remove_subord_capabilities_expr class_defns cond_expr
        , remove_subord_capabilities_block_expr class_defns then_expr
        , remove_subord_capabilities_block_expr class_defns else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        ( loc
        , remove_subord_capabilities_expr class_defns cond_expr
        , remove_subord_capabilities_block_expr class_defns loop_expr )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , remove_subord_capabilities_expr class_defns expr1
        , remove_subord_capabilities_expr class_defns expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, remove_subord_capabilities_expr class_defns expr)

and remove_subord_capabilities_block_expr class_defns
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs = List.map ~f:(remove_subord_capabilities_expr class_defns) exprs in
  Block (loc, type_block_expr, updated_exprs)

let is_this_present obj_vars_and_capabilities =
  let obj_var_names, _, _ = List.unzip3 obj_vars_and_capabilities in
  elem_in_list (Var_name.of_string "this") obj_var_names

(* If we are not in an object method then we can't access subordinate state, so remove
   access to subordinate state. We do this by checking if "this" is a param of the method. *)

let type_subord_capabilities_expr class_defns obj_vars_and_capabilities expr =
  if is_this_present obj_vars_and_capabilities then expr
  else remove_subord_capabilities_expr class_defns expr

let type_subord_capabilities_block_expr class_defns obj_vars_and_capabilities block_expr =
  if is_this_present obj_vars_and_capabilities then block_expr
  else remove_subord_capabilities_block_expr class_defns block_expr

let type_subord_capabilities_method_prototype class_defns obj_class meth_name ret_type
    param_obj_var_capabilities =
  let error_prefix =
    Fmt.str "Potential Data Race in %s's method %s:"
      (Class_name.to_string obj_class)
      (Method_name.to_string meth_name) in
  (* if object is encapsulated - then it's fine to pass subord state in or out of objects
     as all accesses will be via object's owner. *)
  if class_has_mode obj_class Encapsulated class_defns then Ok ()
  else if type_has_mode ret_type Subordinate class_defns then
    Error
      (Error.of_string
         (Fmt.str "%s Subordinate state returned by non-encapsulated method" error_prefix))
  else
    let subord_args =
      List.filter
        ~f:(fun (_, var_class, capabilities) ->
          List.exists
            ~f:(fun capability ->
              capability_fields_have_mode capability var_class Subordinate class_defns)
            capabilities)
        param_obj_var_capabilities in
    if List.is_empty subord_args then Ok ()
    else
      let subord_arg_str =
        String.concat ~sep:", "
          (List.map ~f:(fun (var_name, _, _) -> Var_name.to_string var_name) subord_args)
      in
      Error
        (Error.of_string
           (Fmt.str "%s Subordinate arguments passed into non-encapsulated method: %s"
              error_prefix subord_arg_str))
