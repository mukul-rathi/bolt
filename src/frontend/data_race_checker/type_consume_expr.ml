open Core
open Ast.Ast_types
open Desugaring.Desugared_ast
open Data_race_checker_env

(* Checks whether updating the first id affects the latter - true if no effect, false if
   affected *)
let check_identifiers_disjoint id affected_id =
  match id with
  | Variable (_, var_name, _) -> (
    match affected_id with
    | Variable _ -> not (id = affected_id)
    | ObjField (_, obj_name, _, _, _) -> not (var_name = obj_name) )
  | ObjField _                -> not (id = affected_id)

let remove_reassigned_id reassigned_id consumed_ids =
  List.filter ~f:(check_identifiers_disjoint reassigned_id) consumed_ids

let check_identifier_accessible id consumed_ids =
  List.filter
    ~f:(fun consumed_id -> not (check_identifiers_disjoint consumed_id id))
    consumed_ids
  |> function
  | [] -> Ok ()
  | _  ->
      Error
        (Error.of_string
           (Fmt.str "Type error: Variable %s accessed after being consumed.@."
              (string_of_id id)))

let is_identifier_linear id class_defns =
  match id with
  | Variable (var_type, _, capabilities) -> (
    match var_type with
    (* Check if variable linear *)
    | TEClass var_class ->
        List.exists
          ~f:(fun capability ->
            capability_fields_have_mode capability var_class Linear class_defns)
          capabilities
    | _ -> false )
  | ObjField (_, _, field_type, _, capabilities) ->
      (* check we're accessing a linear field and we have a possible capability through
         which we can access it *)
      let is_field_linear = type_has_mode field_type Linear class_defns in
      is_field_linear && not (List.is_empty capabilities)

let check_identifier_consumable class_defns id consumed_ids =
  let open Result in
  check_identifier_accessible id consumed_ids
  >>= fun () ->
  if is_identifier_linear id class_defns then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "Type error: Trying to consume %s but it is not linear@."
            (string_of_id id)))

let rec check_shared_var_not_consumed var_name = function
  | [] -> Ok ()
  | Variable (_, name, _) :: ids | ObjField (_, name, _, _, _) :: ids ->
      if var_name = name then
        Error
          (Error.of_string
             (Fmt.str "Type error: shared variable %s was consumed."
                (Var_name.to_string var_name)))
      else check_shared_var_not_consumed var_name ids

let rec accumulate_consumed_ids class_defns consumed_ids_acc_res expr =
  let open Result in
  consumed_ids_acc_res
  >>= fun consumed_ids_acc -> type_consume_expr class_defns expr consumed_ids_acc

and type_consume_expr class_defns expr consumed_ids =
  let open Result in
  match expr with
  | Integer _ -> Ok consumed_ids
  | Boolean _ -> Ok consumed_ids
  | Identifier (_, id) ->
      check_identifier_accessible id consumed_ids >>| fun () -> consumed_ids
  | BlockExpr (_, block_expr) ->
      type_consume_block_expr class_defns block_expr consumed_ids
  | Constructor (_, _, _, constructor_args) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(fun acc (ConstructorArg (_, _, expr)) ->
          accumulate_consumed_ids class_defns acc expr)
        constructor_args
  | Let (_, var_type, var_name, bound_expr) ->
      type_consume_expr class_defns bound_expr consumed_ids
      >>| remove_reassigned_id (Variable (var_type, var_name, []))
  | Assign (_, _, identifier, assigned_expr) ->
      type_consume_expr class_defns assigned_expr consumed_ids
      >>| remove_reassigned_id identifier
  | Consume (_, id) ->
      check_identifier_consumable class_defns id consumed_ids
      >>| fun () -> id :: consumed_ids
  | MethodApp (_, obj_type, obj_name, _, _, _, args_exprs) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(accumulate_consumed_ids class_defns)
        args_exprs
      >>= fun updated_consumed_ids ->
      (* check the identifiers the args reduce to have not been already consumed *)
      Result.all_unit
        (List.map
           ~f:(fun arg_expr ->
             Result.all_unit
               (List.map
                  ~f:(fun id -> check_identifier_accessible id updated_consumed_ids)
                  (reduce_expr_to_obj_id arg_expr)))
           args_exprs)
      >>= fun () ->
      (* Check if object hasn't been consumed - i.e. we can call this method *)
      check_identifier_accessible (Variable (obj_type, obj_name, [])) updated_consumed_ids
      >>| fun () -> updated_consumed_ids
  (* For both function and method calls we only locally check if variables consumed - we
     don't abstractly interpret the method/function body *)
  | FunctionApp (_, _, _, args_exprs) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(accumulate_consumed_ids class_defns)
        args_exprs
      >>= fun updated_consumed_ids ->
      (* check the identifiers the args reduce to have not been already consumed *)
      Result.all_unit
        (List.map
           ~f:(fun arg_expr ->
             Result.all_unit
               (List.map
                  ~f:(fun id -> check_identifier_accessible id updated_consumed_ids)
                  (reduce_expr_to_obj_id arg_expr)))
           args_exprs)
      >>| fun () -> updated_consumed_ids
  | Printf (_, _, args_exprs) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(accumulate_consumed_ids class_defns)
        args_exprs
  | FinishAsync (_, _, async_exprs, current_thread_free_vars, curr_thread_expr) ->
      let all_thread_exprs =
        AsyncExpr (current_thread_free_vars, curr_thread_expr) :: async_exprs in
      (* For each thread, check that it doesn't consume any shared variables used by other
         threads *)
      Result.all
        (List.map
           ~f:(fun async_expr ->
             let other_async_exprs =
               List.filter ~f:(fun expr -> not (expr = async_expr)) all_thread_exprs in
             type_consume_async_expr class_defns async_expr other_async_exprs consumed_ids)
           all_thread_exprs)
      >>= fun _ ->
      (* type check each thread individually and aggregate consumed ids (to return) *)
      Result.all
        (List.map
           ~f:(fun (AsyncExpr (_, expr)) ->
             type_consume_block_expr class_defns expr consumed_ids)
           all_thread_exprs)
      >>| fun thread_consumed_id_lists -> List.concat thread_consumed_id_lists
  | If (_, _, cond_expr, then_expr, else_expr) ->
      type_consume_expr class_defns cond_expr consumed_ids
      >>= fun consumed_ids_with_cond ->
      type_consume_block_expr class_defns then_expr consumed_ids_with_cond
      >>= fun consumed_ids_then ->
      type_consume_block_expr class_defns else_expr consumed_ids_with_cond
      >>| fun consumed_ids_else -> consumed_ids_then @ consumed_ids_else
  | While (_, cond_expr, loop_expr) ->
      (* Note we check twice to simulate going through loop multiple times *)
      type_consume_expr class_defns cond_expr consumed_ids
      >>= fun consumed_ids_with_cond1 ->
      type_consume_block_expr class_defns loop_expr consumed_ids_with_cond1
      >>= fun consumed_ids_loop1 ->
      type_consume_expr class_defns cond_expr consumed_ids_loop1
      >>= fun consumed_ids_with_cond2 ->
      type_consume_block_expr class_defns loop_expr consumed_ids_with_cond2
  | BinOp (_, _, _, expr1, expr2) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(accumulate_consumed_ids class_defns)
        [expr1; expr2]
  | UnOp (_, _, _, expr) -> type_consume_expr class_defns expr consumed_ids

and type_consume_block_expr class_defns (Block (_, _, block_exprs)) consumed_ids =
  List.fold ~init:(Ok consumed_ids) ~f:(accumulate_consumed_ids class_defns) block_exprs

and type_consume_async_expr class_defns (AsyncExpr (_, async_expr)) other_async_exprs
    consumed_ids =
  (* Check that any shared variables used by other threads were not consumed by this
     threads *)
  let open Result in
  let shared_variables =
    List.concat_map
      ~f:(fun (AsyncExpr (free_vars_and_types, _)) ->
        List.map ~f:(fun (var_name, _, _) -> var_name) free_vars_and_types)
      other_async_exprs in
  type_consume_block_expr class_defns async_expr consumed_ids
  >>= fun thread_consumed_ids ->
  Result.all_unit
    (List.map
       ~f:(fun shared_var -> check_shared_var_not_consumed shared_var thread_consumed_ids)
       shared_variables)
