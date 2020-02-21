open Core
open Desugared_ast
open Ast.Ast_types

type var_name_map = (Var_name.t * Var_name.t) list

let set_unique_name var_name var_name_map =
  (* prefix with _var since programmer can't set variables starting with _ so this is
     guaranteed to be unique *)
  let num_times_var_shadowed =
    List.length (List.filter ~f:(fun (name, _) -> name = var_name) var_name_map) in
  Var_name.of_string
    (Fmt.str "_var_%s%d" (Var_name.to_string var_name) num_times_var_shadowed)

let rec get_unique_name var_name = function
  | ([] : var_name_map) ->
      Error
        (Error.of_string
           (Fmt.str "Error: no unique var name for (potentially) shadowed variable %s@."
              (Var_name.to_string var_name)))
  | (name, new_name) :: var_name_map ->
      if var_name = name then Ok new_name else get_unique_name var_name var_name_map

let remove_identifier_var_shadowing id var_name_map =
  let open Result in
  match id with
  | Variable (var_type, var_name, regions) ->
      get_unique_name var_name var_name_map
      >>| fun unique_var_name -> Variable (var_type, unique_var_name, regions)
  | ObjField (obj_type, obj_name, field_type, field_name, regions) ->
      get_unique_name obj_name var_name_map
      >>| fun unique_obj_name ->
      ObjField (obj_type, unique_obj_name, field_type, field_name, regions)

let rec remove_var_shadowing_expr expr var_name_map =
  let open Result in
  (* Apply to a list of exprs with independent environments *)
  let map_exprs_remove_var_shadowing_expr exprs =
    Result.all
      (List.map
         ~f:(fun expr ->
           remove_var_shadowing_expr expr var_name_map
           >>| fun (deshadowed_expr, _) -> deshadowed_expr)
         exprs) in
  match expr with
  | Integer _ | Boolean _ -> Ok (expr, var_name_map)
  | Identifier (loc, id) ->
      remove_identifier_var_shadowing id var_name_map
      >>| fun unique_id -> (Identifier (loc, unique_id), var_name_map)
  | BlockExpr (loc, block_expr) ->
      remove_var_shadowing_block_expr block_expr var_name_map
      >>| fun (deshadowed_block_expr, updated_var_name_map) ->
      (BlockExpr (loc, deshadowed_block_expr), updated_var_name_map)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      (* Each constructor arg has a separate environment, so we don't accumulate var maps *)
      Result.all
        (List.map
           ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
             remove_var_shadowing_expr expr var_name_map
             >>| fun (deshadowed_expr, _) ->
             ConstructorArg (type_expr, field_name, deshadowed_expr))
           constructor_args)
      >>| fun deshadowed_constructor_args ->
      (Constructor (loc, type_expr, class_name, deshadowed_constructor_args), var_name_map)
  | Let (loc, type_expr, var_name, bound_expr) ->
      remove_var_shadowing_expr bound_expr var_name_map
      >>| fun (deshadowed_bound_expr, _) ->
      let new_var_name = set_unique_name var_name var_name_map in
      ( Let (loc, type_expr, new_var_name, deshadowed_bound_expr)
      , (var_name, new_var_name) :: var_name_map )
  | Assign (loc, type_expr, id, assigned_expr) ->
      remove_var_shadowing_expr assigned_expr var_name_map
      >>= fun (deshadowed_assigned_expr, _) ->
      remove_identifier_var_shadowing id var_name_map
      >>| fun deshadowed_id ->
      (Assign (loc, type_expr, deshadowed_id, deshadowed_assigned_expr), var_name_map)
  | Consume (loc, id) ->
      remove_identifier_var_shadowing id var_name_map
      >>| fun deshadowed_id -> (Consume (loc, deshadowed_id), var_name_map)
  | MethodApp (loc, type_expr, var_name, obj_type, method_name, args) ->
      map_exprs_remove_var_shadowing_expr args
      >>= fun deshadowed_args ->
      get_unique_name var_name var_name_map
      >>| fun new_var_name ->
      ( MethodApp (loc, type_expr, new_var_name, obj_type, method_name, deshadowed_args)
      , var_name_map )
  | FunctionApp (loc, type_expr, func_name, args) ->
      map_exprs_remove_var_shadowing_expr args
      >>| fun deshadowed_args ->
      (FunctionApp (loc, type_expr, func_name, deshadowed_args), var_name_map)
  | Printf (loc, format_str, args) ->
      map_exprs_remove_var_shadowing_expr args
      >>| fun deshadowed_args -> (Printf (loc, format_str, deshadowed_args), var_name_map)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      Result.all
        (List.map
           ~f:(fun async_expr -> remove_var_shadowing_async_expr async_expr var_name_map)
           async_exprs)
      >>= fun deshadowed_async_exprs ->
      remove_var_shadowing_block_expr curr_thread_expr var_name_map
      >>= fun (deshadowed_curr_thread_expr, _) ->
      Result.all
        (List.map
           ~f:(fun (var, var_type, var_regions) ->
             get_unique_name var var_name_map
             >>| fun new_var_name -> (new_var_name, var_type, var_regions))
           curr_thread_free_vars)
      >>| fun deshadowed_curr_thread_free_vars ->
      ( FinishAsync
          ( loc
          , type_expr
          , deshadowed_async_exprs
          , deshadowed_curr_thread_free_vars
          , deshadowed_curr_thread_expr )
      , var_name_map )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      remove_var_shadowing_expr cond_expr var_name_map
      >>= fun (deshadowed_cond_expr, _) ->
      remove_var_shadowing_block_expr then_expr var_name_map
      >>= fun (deshadowed_then_expr, _) ->
      remove_var_shadowing_block_expr else_expr var_name_map
      >>| fun (deshadowed_else_expr, _) ->
      ( If
          ( loc
          , type_expr
          , deshadowed_cond_expr
          , deshadowed_then_expr
          , deshadowed_else_expr )
      , var_name_map )
  | While (loc, cond_expr, loop_expr) ->
      remove_var_shadowing_expr cond_expr var_name_map
      >>= fun (deshadowed_cond_expr, _) ->
      remove_var_shadowing_block_expr loop_expr var_name_map
      >>| fun (deshadowed_loop_expr, _) ->
      (While (loc, deshadowed_cond_expr, deshadowed_loop_expr), var_name_map)
  | BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      remove_var_shadowing_expr expr1 var_name_map
      >>= fun (deshadowed_expr1, _) ->
      remove_var_shadowing_expr expr2 var_name_map
      >>| fun (deshadowed_expr2, _) ->
      (BinOp (loc, type_expr, bin_op, deshadowed_expr1, deshadowed_expr2), var_name_map)
  | UnOp (loc, type_expr, un_op, expr) ->
      remove_var_shadowing_expr expr var_name_map
      >>| fun (deshadowed_expr, _) ->
      (UnOp (loc, type_expr, un_op, deshadowed_expr), var_name_map)

and remove_var_shadowing_block_expr (Block (loc, type_expr, exprs)) var_name_map =
  let open Result in
  (* Pass any mapping from previous let expressions in block to subsequent expressions *)
  List.fold
    ~init:(Ok ([], var_name_map))
    ~f:(fun res expr ->
      res
      >>= fun (deshadowed_exprs, acc_var_name_map) ->
      remove_var_shadowing_expr expr acc_var_name_map
      >>| fun (deshadowed_expr, updated_acc_var_name_map) ->
      (deshadowed_expr :: deshadowed_exprs, updated_acc_var_name_map))
    exprs
  >>| fun (rev_deshadowed_exprs, _) ->
  (* note we consed on front of list so reversed the order *)
  (* the accumulated var map is block scoped, so drop it *)
  (Block (loc, type_expr, List.rev rev_deshadowed_exprs), var_name_map)

and remove_var_shadowing_async_expr (AsyncExpr (free_vars, block_expr)) var_name_map =
  let open Result in
  Result.all
    (List.map
       ~f:(fun (var, var_type, var_regions) ->
         get_unique_name var var_name_map
         >>| fun new_var_name -> (new_var_name, var_type, var_regions))
       free_vars)
  >>= fun deshadowed_free_vars ->
  remove_var_shadowing_block_expr block_expr var_name_map
  >>| fun (deshadowed_body_expr, _) ->
  AsyncExpr (deshadowed_free_vars, deshadowed_body_expr)

let rec init_var_map_from_params = function
  | [] -> []
  | TParam (_, param_name, _) :: params ->
      (param_name, param_name) :: init_var_map_from_params params

let remove_var_shadowing_method_defn
    (TMethod (method_name, return_type, params, region_effects, body_expr)) =
  let open Result in
  let this_var = Var_name.of_string "this" in
  remove_var_shadowing_block_expr body_expr
    ((this_var, this_var) :: init_var_map_from_params params)
  >>| fun (deshadowed_body_expr, _) ->
  TMethod (method_name, return_type, params, region_effects, deshadowed_body_expr)

let remove_var_shadowing_class_defn
    (TClass (class_name, region_defns, field_defns, method_defns)) =
  let open Result in
  Result.all (List.map ~f:remove_var_shadowing_method_defn method_defns)
  >>| fun deshadowed_method_defns ->
  TClass (class_name, region_defns, field_defns, deshadowed_method_defns)

let remove_var_shadowing_function_defn
    (TFunction (func_name, return_type, params, body_expr)) =
  let open Result in
  remove_var_shadowing_block_expr body_expr (init_var_map_from_params params)
  >>| fun (deshadowed_body_expr, _) ->
  TFunction (func_name, return_type, params, deshadowed_body_expr)

let remove_var_shadowing_program (Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all (List.map ~f:remove_var_shadowing_class_defn class_defns)
  >>= fun deshadowed_class_defns ->
  Result.all (List.map ~f:remove_var_shadowing_function_defn function_defns)
  >>= fun deshadowed_function_defns ->
  remove_var_shadowing_block_expr main_expr []
  >>| fun (deshadowed_main_expr, _) ->
  Prog (deshadowed_class_defns, deshadowed_function_defns, deshadowed_main_expr)
