open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types

let aggregate_capability_accesses_thread_free_var all_vars_capability_accesses
    (obj_name, obj_class, _) =
  List.filter_map
    ~f:(fun (name, class_name, var_capabilities_accessed) ->
      if obj_name = name && class_name = obj_class then Some var_capabilities_accessed
      else None)
    all_vars_capability_accesses
  |> fun updated_capabilities_accessed ->
  List.dedup_and_sort
    ~compare:(fun a b -> if a = b then 0 else 1)
    (List.concat updated_capabilities_accessed)
  |> fun deduped_updated_capabilities_accessed ->
  (obj_name, obj_class, deduped_updated_capabilities_accessed)

let get_arg_capabilities_used_by_fn class_defns param arg =
  match
    (param_to_obj_var_and_capabilities class_defns param, reduce_expr_to_obj_ids arg)
  with
  | Some (_, _, arg_capabilities_used), possible_expr_reduced_ids ->
      List.filter_map
        ~f:(function
          | Variable (var_type, var_name, _, _) -> (
            match var_type with
            | TEClass (obj_class, _) -> Some (var_name, obj_class, arg_capabilities_used)
            | _                      -> None )
          | ObjField _ ->
              (* If passing in a field of an object, the capabilities required are that of
                 the fields, not the object itself so we don't track. *)
              None)
        possible_expr_reduced_ids
  | None, _ -> []

let use_all_identifier_capabilities id =
  match id with
  | Variable (var_type, var_name, capabilities, _) -> (
    match var_type with
    | TEClass (obj_class, _) -> [(var_name, obj_class, capabilities)]
    | _                      -> [] )
  | ObjField (obj_class, obj_name, _, _, capabilities, _) ->
      [(obj_name, obj_class, capabilities)]

let choose_identifier_capabilities id =
  (* prefer capabilities with non-linear modes, as least restrictive *)
  match id with
  | Variable _ ->
      (* just referencing a variable doesn't require us to use capabilities - it's only if
         we access a field or call a method. *)
      []
  | ObjField (obj_class, obj_name, _, _, capabilities, _) ->
      ( match
          List.find ~f:(fun (TCapability (mode, _)) -> not (mode = Linear)) capabilities
        with
      | Some capability -> [capability]
      | None            ->
          if
            List.is_empty capabilities
            (* Don't worry if no capability found - this will be caught in later
               type-checking. *)
          then []
          else [List.hd_exn capabilities] )
      |> fun chosen_capabilities -> [(obj_name, obj_class, chosen_capabilities)]

let rec aggregate_capability_accesses_expr class_defns function_defns expr =
  let aggregate_capability_accesses_expr_rec =
    aggregate_capability_accesses_expr class_defns function_defns in
  let aggregate_capability_accesses_block_expr_rec =
    aggregate_capability_accesses_block_expr class_defns function_defns in
  match expr with
  | Integer _ | Boolean _ -> (expr, [])
  | Identifier (loc, id) ->
      choose_identifier_capabilities id
      |> fun capability_accesses -> (Identifier (loc, id), capability_accesses)
  | BlockExpr (loc, block_expr) ->
      aggregate_capability_accesses_block_expr_rec block_expr
      |> fun (updated_block, capability_accesses) ->
      (BlockExpr (loc, updated_block), capability_accesses)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      List.unzip
        (List.map
           ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
             aggregate_capability_accesses_expr_rec expr
             |> fun (updated_expr, arg_capability_accesses) ->
             ( ConstructorArg (type_expr, field_name, updated_expr)
             , arg_capability_accesses ))
           constructor_args)
      |> fun (updated_args, args_capability_accesses) ->
      ( Constructor (loc, type_expr, class_name, updated_args)
      , List.concat args_capability_accesses )
  | Let (loc, type_expr, var_name, bound_expr) ->
      aggregate_capability_accesses_expr_rec bound_expr
      |> fun (updated_bound_expr, capability_accesses) ->
      (Let (loc, type_expr, var_name, updated_bound_expr), capability_accesses)
  | Assign (loc, type_expr, id, assigned_expr) ->
      choose_identifier_capabilities id
      |> fun id_capability_accesses ->
      aggregate_capability_accesses_expr_rec assigned_expr
      |> fun (updated_assigned_expr, expr_capability_accesses) ->
      ( Assign (loc, type_expr, id, updated_assigned_expr)
      , id_capability_accesses @ expr_capability_accesses )
  | Consume (loc, id) ->
      use_all_identifier_capabilities id
      |> fun id_capability_accesses -> (Consume (loc, id), id_capability_accesses)
  | MethodApp (loc, type_expr, obj_name, obj_capabilities, obj_class, method_name, args)
    ->
      List.unzip
        (List.map
           ~f:(fun (param, arg) ->
             aggregate_capability_accesses_expr_rec arg
             |> fun (updated_arg, arg_capability_accesses) ->
             ( updated_arg
             , get_arg_capabilities_used_by_fn class_defns param arg
               @ arg_capability_accesses ))
           (List.zip_exn (get_method_params obj_class method_name class_defns) args))
      |> fun (updated_args, args_capability_accesses) ->
      get_method_capabilities_used obj_class method_name class_defns
      |> fun obj_method_capabilities_used ->
      ( MethodApp
          ( loc
          , type_expr
          , obj_name
          , obj_capabilities
          , obj_class
          , method_name
          , updated_args )
      , (obj_name, obj_class, obj_method_capabilities_used)
        :: List.concat args_capability_accesses )
  | FunctionApp (loc, return_type, func_name, args) ->
      List.unzip
        (List.map
           ~f:(fun (param, arg) ->
             aggregate_capability_accesses_expr_rec arg
             |> fun (updated_arg, arg_capability_accesses) ->
             ( updated_arg
             , get_arg_capabilities_used_by_fn class_defns param arg
               @ arg_capability_accesses ))
           (List.zip_exn (get_function_params func_name function_defns) args))
      |> fun (updated_args, args_capability_accesses) ->
      ( FunctionApp (loc, return_type, func_name, updated_args)
      , List.concat args_capability_accesses )
  | Printf (loc, format_str, args) ->
      List.unzip
        (List.map
           ~f:(fun arg ->
             aggregate_capability_accesses_expr_rec arg
             |> fun (updated_arg, arg_capability_accesses) ->
             (updated_arg, arg_capability_accesses))
           args)
      |> fun (updated_args, args_capability_accesses) ->
      (Printf (loc, format_str, updated_args), List.concat args_capability_accesses)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      List.unzip
        (List.map
           ~f:(fun (AsyncExpr (free_vars, expr)) ->
             aggregate_capability_accesses_block_expr_rec expr
             |> fun (updated_expr, expr_capability_accesses) ->
             List.map
               ~f:(aggregate_capability_accesses_thread_free_var expr_capability_accesses)
               free_vars
             |> fun updated_free_vars ->
             (AsyncExpr (updated_free_vars, updated_expr), expr_capability_accesses))
           async_exprs)
      |> fun (updated_async_exprs, async_exprs_capability_accesses) ->
      aggregate_capability_accesses_block_expr_rec curr_thread_expr
      |> fun (updated_curr_thread_expr, curr_thread_capability_accesses) ->
      List.map
        ~f:(aggregate_capability_accesses_thread_free_var curr_thread_capability_accesses)
        curr_thread_free_vars
      |> fun updated_curr_thread_free_vars ->
      ( FinishAsync
          ( loc
          , type_expr
          , updated_async_exprs
          , updated_curr_thread_free_vars
          , updated_curr_thread_expr )
      , curr_thread_capability_accesses @ List.concat async_exprs_capability_accesses )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      aggregate_capability_accesses_expr_rec cond_expr
      |> fun (updated_cond_expr, cond_capability_accesses) ->
      aggregate_capability_accesses_block_expr_rec then_expr
      |> fun (updated_then_expr, then_capability_accesses) ->
      aggregate_capability_accesses_block_expr_rec else_expr
      |> fun (updated_else_expr, else_capability_accesses) ->
      ( If (loc, type_expr, updated_cond_expr, updated_then_expr, updated_else_expr)
      , cond_capability_accesses @ then_capability_accesses @ else_capability_accesses )
  | While (loc, cond_expr, loop_expr) ->
      aggregate_capability_accesses_expr_rec cond_expr
      |> fun (updated_cond_expr, cond_capability_accesses) ->
      aggregate_capability_accesses_block_expr_rec loop_expr
      |> fun (updated_loop_expr, loop_capability_accesses) ->
      ( While (loc, updated_cond_expr, updated_loop_expr)
      , cond_capability_accesses @ loop_capability_accesses )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      aggregate_capability_accesses_expr_rec expr1
      |> fun (updated_expr1, expr1_capability_accesses) ->
      aggregate_capability_accesses_expr_rec expr2
      |> fun (updated_expr2, expr2_capability_accesses) ->
      ( BinOp (loc, type_expr, binop, updated_expr1, updated_expr2)
      , expr1_capability_accesses @ expr2_capability_accesses )
  | UnOp (loc, type_expr, unop, expr) ->
      aggregate_capability_accesses_expr_rec expr
      |> fun (updated_expr, expr_capability_accesses) ->
      (UnOp (loc, type_expr, unop, updated_expr), expr_capability_accesses)

and aggregate_capability_accesses_block_expr class_defns function_defns
    (Block (loc, type_block_expr, exprs)) =
  List.unzip
    (List.map ~f:(aggregate_capability_accesses_expr class_defns function_defns) exprs)
  |> fun (updated_exprs, capability_accesses) ->
  (Block (loc, type_block_expr, updated_exprs), List.concat capability_accesses)
