open Core
open Desugaring.Desugared_ast
open Ast.Ast_types
open Data_race_checker_env

(* Check if the expression is reduced to an id that matches a given name_to_match *)
let var_in_expr_reduced_ids name_to_match ids =
  List.length
    (List.filter
       ~f:(function
         | Variable (_, name, _) -> name_to_match = name
         | ObjField (_, obj_name, _, _, _) -> name_to_match = obj_name)
       ids)
  > 0

let rec find_aliases_in_block_expr name_to_match curr_aliases
    (Block (loc, type_block_expr, exprs)) =
  match exprs with
  | []                      -> curr_aliases
  | expr :: remaining_exprs ->
      let expr_aliases =
        match expr with
        | Let (_, _, name, bound_expr) ->
            let expr_reduced_ids = reduce_expr_to_obj_id bound_expr in
            if
              List.exists
                ~f:(fun name -> var_in_expr_reduced_ids name expr_reduced_ids)
                (name_to_match :: curr_aliases)
            then name :: curr_aliases
            else curr_aliases
        | _                            -> curr_aliases in
      let other_exprs_aliases =
        find_aliases_in_block_expr name_to_match curr_aliases
          (Block (loc, type_block_expr, remaining_exprs)) in
      List.concat [expr_aliases; other_exprs_aliases]

let maybe_update_capabilities (name_to_match : Var_name.t) capability_filter_fn name
    capabilities =
  if name = name_to_match then
    List.filter ~f:(capability_filter_fn capabilities) capabilities
  else capabilities

let update_identifier_capabilities name_to_match capability_filter_fn id =
  match id with
  | Variable (var_type, name, capabilities) ->
      Variable
        ( var_type
        , name
        , maybe_update_capabilities name_to_match capability_filter_fn name capabilities
        )
  | ObjField (obj_class, obj_name, field_type, field_name, capabilities) ->
      ObjField
        ( obj_class
        , obj_name
        , field_type
        , field_name
        , maybe_update_capabilities name_to_match capability_filter_fn obj_name
            capabilities )

let rec update_identifier_capabilities_expr name_to_match capability_filter_fn expr =
  let update_identifier_capabilities_expr_rec =
    update_identifier_capabilities_expr name_to_match capability_filter_fn in
  let update_identifier_capabilities_block_expr_rec =
    update_identifier_capabilities_block_expr name_to_match capability_filter_fn in
  let update_var_modes_identifier_rec =
    update_identifier_capabilities name_to_match capability_filter_fn in
  match expr with
  | Integer _ | Boolean _ -> expr
  | Identifier (loc, id) -> Identifier (loc, update_var_modes_identifier_rec id)
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, update_identifier_capabilities_block_expr_rec block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg
              (type_expr, field_name, update_identifier_capabilities_expr_rec expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_args)
  | Let (loc, type_expr, name_to_match, bound_expr) ->
      Let
        (loc, type_expr, name_to_match, update_identifier_capabilities_expr_rec bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign
        ( loc
        , type_expr
        , update_var_modes_identifier_rec id
        , update_identifier_capabilities_expr_rec assigned_expr )
  | Consume (loc, id) -> Consume (loc, update_var_modes_identifier_rec id)
  | MethodApp (loc, type_expr, obj_name, obj_capabilities, obj_class, method_name, args)
    ->
      MethodApp
        ( loc
        , type_expr
        , obj_name
        , maybe_update_capabilities name_to_match capability_filter_fn obj_name
            obj_capabilities
        , obj_class
        , method_name
        , List.map ~f:update_identifier_capabilities_expr_rec args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp
        ( loc
        , return_type
        , func_name
        , List.map ~f:update_identifier_capabilities_expr_rec args )
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:update_identifier_capabilities_expr_rec args)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, expr)) ->
              AsyncExpr (free_vars, update_identifier_capabilities_block_expr_rec expr))
            async_exprs
        , curr_thread_free_vars
        , update_identifier_capabilities_block_expr_rec curr_thread_expr )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , update_identifier_capabilities_expr_rec cond_expr
        , update_identifier_capabilities_block_expr_rec then_expr
        , update_identifier_capabilities_block_expr_rec else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        ( loc
        , update_identifier_capabilities_expr_rec cond_expr
        , update_identifier_capabilities_block_expr_rec loop_expr )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , update_identifier_capabilities_expr_rec expr1
        , update_identifier_capabilities_expr_rec expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, update_identifier_capabilities_expr_rec expr)

and update_identifier_capabilities_block_expr name_to_match capability_filter_fn
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs =
    List.map
      ~f:(update_identifier_capabilities_expr name_to_match capability_filter_fn)
      exprs in
  let updated_block_expr = Block (loc, type_block_expr, updated_exprs) in
  (* we propagate any capability constraints to aliases *)
  List.fold ~init:updated_block_expr
    ~f:(fun acc_block_expr alias ->
      update_identifier_capabilities_block_expr alias capability_filter_fn acc_block_expr)
    (find_aliases_in_block_expr name_to_match [] updated_block_expr)
