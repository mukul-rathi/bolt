open Core
open Typing.Typed_ast
open Ast.Ast_types
open Desugar_env

let remove_bound_var bound_var_name free_vars_list =
  List.filter ~f:(fun (var_name, _, _) -> not (var_name = bound_var_name)) free_vars_list

let union_free_vars_lists free_vars_lists =
  List.dedup_and_sort
    ~compare:(fun name_1 name_2 -> if name_1 = name_2 then 0 else 1)
    (List.concat free_vars_lists)

let free_obj_vars_identifier class_defns = function
  | Variable (var_type, var_name) -> (
    match var_type with
    | TEClass (class_name, _) ->
        [(var_name, class_name, get_class_regions class_name class_defns)]
    | _                       -> [] )
  | ObjField (obj_class, obj_name, _, obj_field) ->
      [(obj_name, obj_class, get_class_field_regions obj_class obj_field class_defns)]

let rec free_obj_vars_expr class_defns = function
  | Integer _ -> []
  | Boolean _ -> []
  | Identifier (_, id) -> free_obj_vars_identifier class_defns id
  | BlockExpr (_, block_expr) -> free_obj_vars_block_expr class_defns block_expr
  | Constructor (_, _, _, constructor_args) ->
      union_free_vars_lists
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) -> free_obj_vars_expr class_defns expr)
           constructor_args)
  | Let (_, _, _, bound_expr) -> free_obj_vars_expr class_defns bound_expr
  | Assign (_, _, identifier, assigned_expr) ->
      free_obj_vars_expr class_defns assigned_expr
      |> fun free_vars_assigned_expr ->
      free_obj_vars_identifier class_defns identifier @ free_vars_assigned_expr
  | Consume (_, id) -> free_obj_vars_identifier class_defns id
  | MethodApp (_, _, obj_name, obj_class, _, args_exprs) ->
      (obj_name, obj_class, get_class_regions obj_class class_defns)
      :: union_free_vars_lists (List.map ~f:(free_obj_vars_expr class_defns) args_exprs)
  | FunctionApp (_, _, _, args_exprs) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr class_defns) args_exprs)
  | Printf (_, _, args_exprs) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr class_defns) args_exprs)
  | FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      let free_vars_async_exprs =
        List.map
          ~f:(fun (AsyncExpr block_expr) ->
            free_obj_vars_block_expr class_defns block_expr)
          async_exprs in
      union_free_vars_lists
        (free_obj_vars_block_expr class_defns curr_thread_expr :: free_vars_async_exprs)
  | If (_, _, cond_expr, then_expr, else_expr) ->
      union_free_vars_lists
        [ free_obj_vars_expr class_defns cond_expr
        ; free_obj_vars_block_expr class_defns then_expr
        ; free_obj_vars_block_expr class_defns else_expr ]
  | While (_, cond_expr, loop_expr) ->
      union_free_vars_lists
        [ free_obj_vars_expr class_defns cond_expr
        ; free_obj_vars_block_expr class_defns loop_expr ]
  | BinOp (_, _, _, expr1, expr2) ->
      union_free_vars_lists (List.map ~f:(free_obj_vars_expr class_defns) [expr1; expr2])
  | UnOp (_, _, _, expr) -> free_obj_vars_expr class_defns expr

and free_obj_vars_block_expr class_defns (Block (loc, block_type, block_exprs)) =
  match block_exprs with
  | []            -> []
  | expr :: exprs -> (
      free_obj_vars_block_expr class_defns (Block (loc, block_type, exprs))
      |> fun exprs_free_vars ->
      match expr with
      (* If let binding then need to remove bound variable from block's free vars *)
      | Let (_, _, var_name, bound_expr) ->
          free_obj_vars_expr class_defns bound_expr
          |> fun bound_expr_free_vars ->
          union_free_vars_lists
            [bound_expr_free_vars; remove_bound_var var_name exprs_free_vars]
      | _ ->
          free_obj_vars_expr class_defns expr
          |> fun expr_free_vars -> union_free_vars_lists [expr_free_vars; exprs_free_vars]
      )
