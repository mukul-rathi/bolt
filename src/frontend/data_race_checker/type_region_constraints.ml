open Core
open Desugaring.Desugared_ast
open Ast.Ast_types
open Update_identifier_regions
open Data_race_checker_env

(* We check that the regions can be accessed concurrently *)
let type_concurrent_region_pair_constraints_var class_defns obj_class obj_name
    regions_thread1 regions_thread2 loc =
  Result.all_unit
    (List.map
       ~f:(fun (TRegion (_, region_thread1_name) as region_thread1) ->
         Result.all_unit
           (List.map
              ~f:(fun (TRegion (_, region_thread2_name) as region_thread2) ->
                if
                  can_concurrently_access_regions obj_class class_defns region_thread1
                    region_thread2
                then Ok ()
                else
                  Error
                    (Error.of_string
                       (Fmt.str
                          "Potential data race: %s Can't access regions %s and %s of object %s concurrently@."
                          (string_of_loc loc)
                          (Region_name.to_string region_thread1_name)
                          (Region_name.to_string region_thread2_name)
                          (Var_name.to_string obj_name))))
              regions_thread2))
       regions_thread1)

let rec type_concurrent_regions_constraints_var class_defns obj_name obj_class
    all_threads_regions loc =
  match all_threads_regions with
  | [] -> Ok ()
  | thread_1_regions :: other_threads_regions ->
      let open Result in
      Result.all_unit
        (List.map
           ~f:(fun thread_2_regions ->
             type_concurrent_region_pair_constraints_var class_defns obj_class obj_name
               thread_1_regions thread_2_regions loc)
           other_threads_regions)
      >>= fun () ->
      type_concurrent_regions_constraints_var class_defns obj_name obj_class
        other_threads_regions loc

let type_concurrent_region_constraints_all_vars class_defns threads_free_vars loc =
  let var_names_and_classes =
    List.dedup_and_sort
      ~compare:(fun a b -> if a = b then 0 else 1)
      (List.map
         ~f:(fun (var_name, class_name, _) -> (var_name, class_name))
         threads_free_vars) in
  Result.all_unit
    (List.map (* check constraint for each object *)
       ~f:(fun (obj_name, obj_class) ->
         List.filter_map
           ~f:(fun (var_name, class_name, regions) ->
             if var_name = obj_name && class_name = obj_class then Some regions else None)
           threads_free_vars
         |> fun all_threads_obj_regions ->
         type_concurrent_regions_constraints_var class_defns obj_name obj_class
           all_threads_obj_regions loc)
       var_names_and_classes)

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
  | FinishAsync (loc, _, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      let all_async_free_vars =
        List.map ~f:(fun (AsyncExpr (async_free_vars, _)) -> async_free_vars) async_exprs
      in
      type_concurrent_region_constraints_all_vars class_defns
        (curr_thread_free_vars @ List.concat all_async_free_vars)
        loc
      >>= fun () ->
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
