open Core
open Ast.Ast_types
open Free_vars_expr
open Data_race_checker_ast

(* Checks whether updating the first id affects the latter - true if no effect, false if
   affected *)
let check_identifiers_disjoint id affected_id =
  match id with
  | Variable (_, var_name) -> (
    match affected_id with
    | Variable _                   -> not (id = affected_id)
    | ObjField (_, obj_name, _, _) -> not (var_name = obj_name) )
  | ObjField _             -> not (id = affected_id)

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

let rec check_shared_var_not_consumed var_name = function
  | [] -> Ok ()
  | Variable (_, name) :: ids | ObjField (_, name, _, _) :: ids ->
      if var_name = name then
        Error
          (Error.of_string
             (Fmt.str "Type error: shared variable %s was consumed."
                (Var_name.to_string var_name)))
      else check_shared_var_not_consumed var_name ids

let rec accumulate_consumed_ids consumed_ids_acc_res expr =
  let open Result in
  consumed_ids_acc_res >>= fun consumed_ids_acc -> type_consume_expr expr consumed_ids_acc

and type_consume_expr expr consumed_ids =
  let open Result in
  match expr with
  | Integer _ -> Ok consumed_ids
  | Boolean _ -> Ok consumed_ids
  | Identifier (_, id) ->
      check_identifier_accessible id consumed_ids >>| fun () -> consumed_ids
  | BlockExpr (_, block_expr) -> type_consume_block_expr block_expr consumed_ids
  | Constructor (_, _, _, constructor_args) ->
      List.fold ~init:(Ok consumed_ids)
        ~f:(fun acc (ConstructorArg (_, _, expr)) -> accumulate_consumed_ids acc expr)
        constructor_args
  | Let (_, var_type, var_name, bound_expr) ->
      type_consume_expr bound_expr consumed_ids
      >>| remove_reassigned_id (Variable (var_type, var_name))
  | Assign (_, _, identifier, assigned_expr) ->
      type_consume_expr assigned_expr consumed_ids >>| remove_reassigned_id identifier
  | Consume (_, id) ->
      check_identifier_accessible id consumed_ids >>| fun () -> id :: consumed_ids
  | MethodApp (_, obj_type, obj_name, _, _, args_exprs) ->
      (* Check if object hasn't been consumed - i.e. we can call this method *)
      check_identifier_accessible (Variable (obj_type, obj_name)) consumed_ids
      >>= fun () ->
      List.fold ~init:(Ok consumed_ids) ~f:accumulate_consumed_ids args_exprs
  (* For both function and method calls we only locally check if variables consumed - we
     don't abstractly interpret the method/function body *)
  | FunctionApp (_, _, _, args_exprs) ->
      List.fold ~init:(Ok consumed_ids) ~f:accumulate_consumed_ids args_exprs
  | Printf (_, _, args_exprs) ->
      List.fold ~init:(Ok consumed_ids) ~f:accumulate_consumed_ids args_exprs
  | FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      let all_thread_exprs =
        curr_thread_expr
        :: List.map ~f:(fun (AsyncExpr (_, block_expr)) -> block_expr) async_exprs in
      (* type check each thread individually and aggregate consumed ids *)
      Result.all
        (List.map
           ~f:(fun expr -> type_consume_block_expr expr consumed_ids)
           all_thread_exprs)
      >>= fun thread_consumed_id_lists ->
      Ok (List.concat thread_consumed_id_lists)
      >>= fun all_thread_consumed_ids ->
      (* For each thread, check that it doesn't consume any shared variables used by other
         threads *)
      Result.all
        (List.map
           ~f:(fun async_expr ->
             let other_async_exprs =
               List.filter ~f:(fun expr -> not (expr = async_expr)) all_thread_exprs in
             type_consume_async_expr async_expr other_async_exprs consumed_ids)
           all_thread_exprs)
      >>| fun _ -> all_thread_consumed_ids
  | If (_, _, cond_expr, then_expr, else_expr) ->
      type_consume_expr cond_expr consumed_ids
      >>= fun consumed_ids_with_cond ->
      type_consume_block_expr then_expr consumed_ids_with_cond
      >>= fun consumed_ids_then ->
      type_consume_block_expr else_expr consumed_ids_with_cond
      >>| fun consumed_ids_else -> consumed_ids_then @ consumed_ids_else
  | While (_, cond_expr, loop_expr) ->
      (* Note we check twice to simulate going through loop multiple times *)
      type_consume_expr cond_expr consumed_ids
      >>= fun consumed_ids_with_cond1 ->
      type_consume_block_expr loop_expr consumed_ids_with_cond1
      >>= fun consumed_ids_loop1 ->
      type_consume_expr cond_expr consumed_ids_loop1
      >>= fun consumed_ids_with_cond2 ->
      type_consume_block_expr loop_expr consumed_ids_with_cond2
  | BinOp (_, _, _, expr1, expr2) ->
      List.fold ~init:(Ok consumed_ids) ~f:accumulate_consumed_ids [expr1; expr2]
  | UnOp (_, _, _, expr) -> type_consume_expr expr consumed_ids

and type_consume_block_expr (Block (_, _, block_exprs)) consumed_ids =
  List.fold ~init:(Ok consumed_ids) ~f:accumulate_consumed_ids block_exprs

and type_consume_async_expr async_expr other_async_exprs consumed_ids =
  (* Check that any shared variables used by other threads were not consumed by this
     threads *)
  let open Result in
  let shared_variables = List.concat_map ~f:free_vars_block_expr other_async_exprs in
  type_consume_block_expr async_expr consumed_ids
  >>= fun thread_consumed_ids ->
  Result.all_unit
    (List.map
       ~f:(fun shared_var -> check_shared_var_not_consumed shared_var thread_consumed_ids)
       shared_variables)
