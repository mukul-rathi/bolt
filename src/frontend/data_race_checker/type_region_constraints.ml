open Core
open Desugaring.Desugared_ast
open Ast.Ast_types
open Update_identifier_regions
open Data_race_checker_env

let type_param_region_constraints obj_vars_and_regions block_expr =
  List.fold ~init:block_expr
    ~f:(fun acc_expr (obj_var_name, _, regions) ->
      update_identifier_regions_block_expr obj_var_name
        (fun _ region -> elem_in_list region regions)
        acc_expr)
    obj_vars_and_regions

(* since tracking aliasing once an expression is assigned to a field of an object is
   intractable, we require that if we assign an expression to a field, that all regions
   are available to the field being assigned to. *)
let type_region_constraints_assigned_expr class_defns type_expr assign_expr loc =
  let assign_expr_reduced_ids = reduce_expr_to_obj_id assign_expr in
  let ids_satisfy_region_constraints =
    List.for_all
      ~f:(fun reduced_id ->
        match (type_expr, reduced_id) with
        | TEClass (class_name, _), Variable (_, _, var_regions) ->
            let required_regions = get_class_regions class_name class_defns in
            is_subset_of required_regions var_regions
        | _ -> true)
      assign_expr_reduced_ids in
  if ids_satisfy_region_constraints then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "%s Assigned expression doesn't have all regions available@."
            (string_of_loc loc)))

let type_region_constraints_function_arg class_defns function_str loc (param, arg) =
  let _, _, param_regions =
    List.unzip3 (params_to_obj_vars_and_regions class_defns [param]) in
  let possible_reduced_arg_ids = reduce_expr_to_obj_id arg in
  if
    List.for_all
      ~f:(function
        | Variable (_, _, var_regions) ->
            is_subset_of (List.concat param_regions) var_regions
        | ObjField _                   -> true)
      possible_reduced_arg_ids
  then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str
            "%s Potential data race: %s's argument region constraints not satisfied."
            (string_of_loc loc) function_str))

let type_regions_constraints_identifier id loc =
  let error_msg =
    Error
      (Error.of_string
         (Fmt.str "%s Potential data race: no allowed regions for %s@."
            (string_of_loc loc) (string_of_id id))) in
  match id with
  | Variable (var_type, _, regions) -> (
    match var_type with
    | TEClass _ -> if List.is_empty regions then error_msg else Ok ()
    | _         -> Ok () )
  | ObjField (_, _, _, _, regions) -> if List.is_empty regions then error_msg else Ok ()

let rec type_regions_constraints_expr class_defns function_defns expr =
  let open Result in
  match expr with
  | Integer _ | Boolean _ -> Ok ()
  | Identifier (loc, id) -> type_regions_constraints_identifier id loc
  | BlockExpr (_, block_expr) ->
      (type_regions_constraints_block_expr class_defns function_defns) block_expr
  | Constructor (_, _, _, constructor_args) ->
      Result.all_unit
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) ->
             (type_regions_constraints_expr class_defns function_defns) expr)
           constructor_args)
  | Let (_, _, _, bound_expr) ->
      (type_regions_constraints_expr class_defns function_defns) bound_expr
  | Assign (loc, type_expr, id, assigned_expr) ->
      type_regions_constraints_identifier id loc
      >>= fun () ->
      type_region_constraints_assigned_expr class_defns type_expr assigned_expr loc
      >>= fun () ->
      (type_regions_constraints_expr class_defns function_defns) assigned_expr
  | Consume (loc, id) -> type_regions_constraints_identifier id loc
  | MethodApp (loc, _, obj_name, obj_class, meth_name, args) ->
      let params = get_method_params obj_class meth_name class_defns in
      let method_str =
        Fmt.str "Obj %s's method %s" (Var_name.to_string obj_name)
          (Method_name.to_string meth_name) in
      Result.all_unit
        (List.map
           ~f:(type_region_constraints_function_arg class_defns method_str loc)
           (List.zip_exn params args))
      >>= fun () ->
      Result.all_unit
        (List.map ~f:(type_regions_constraints_expr class_defns function_defns) args)
  | FunctionApp (loc, _, func_name, args) ->
      let params = get_function_params func_name function_defns in
      let function_str = Fmt.str "Function %s" (Function_name.to_string func_name) in
      Result.all_unit
        (List.map
           ~f:(type_region_constraints_function_arg class_defns function_str loc)
           (List.zip_exn params args))
      >>= fun () ->
      Result.all_unit
        (List.map ~f:(type_regions_constraints_expr class_defns function_defns) args)
  | Printf (_, _, args) ->
      Result.all_unit
        (List.map ~f:(type_regions_constraints_expr class_defns function_defns) args)
  | FinishAsync (_, _, async_exprs, _, curr_thread_expr) ->
      Result.all_unit
        (List.map
           ~f:(fun (AsyncExpr (_, expr)) ->
             (type_regions_constraints_block_expr class_defns function_defns) expr)
           async_exprs)
      >>= fun () ->
      (type_regions_constraints_block_expr class_defns function_defns) curr_thread_expr
  | If (_, _, cond_expr, then_expr, else_expr) ->
      (type_regions_constraints_expr class_defns function_defns) cond_expr
      >>= fun () ->
      (type_regions_constraints_block_expr class_defns function_defns) then_expr
      >>= fun () ->
      (type_regions_constraints_block_expr class_defns function_defns) else_expr
  | While (_, cond_expr, loop_expr) ->
      (type_regions_constraints_expr class_defns function_defns) cond_expr
      >>= fun () ->
      (type_regions_constraints_block_expr class_defns function_defns) loop_expr
  | BinOp (_, _, _, expr1, expr2) ->
      (type_regions_constraints_expr class_defns function_defns) expr1
      >>= fun () -> (type_regions_constraints_expr class_defns function_defns) expr2
  | UnOp (_, _, _, expr) ->
      (type_regions_constraints_expr class_defns function_defns) expr

and type_regions_constraints_block_expr class_defns function_defns (Block (_, _, exprs)) =
  Result.all_unit
    (List.map ~f:(type_regions_constraints_expr class_defns function_defns) exprs)
