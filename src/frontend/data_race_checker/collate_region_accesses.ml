open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types

let collate_region_accesses_thread_free_var all_vars_region_accesses
    (var_name, obj_class, _) =
  List.filter_map
    ~f:(fun (name, class_name, var_regions_accessed) ->
      if var_name = name && class_name = obj_class then Some var_regions_accessed
      else None)
    all_vars_region_accesses
  |> fun updated_regions_accessed ->
  List.dedup_and_sort
    ~compare:(fun a b -> if a = b then 0 else 1)
    (List.concat updated_regions_accessed)
  |> fun deduped_updated_regions_accessed ->
  (var_name, obj_class, deduped_updated_regions_accessed)

let get_arg_regions_used_by_fn class_defns param arg =
  match (param_to_obj_var_and_regions class_defns param, reduce_expr_to_obj_id arg) with
  | Some (_, _, arg_regions_used), possible_expr_reduced_ids ->
      List.filter_map
        ~f:(function
          | Variable (var_type, var_name, _) -> (
            match var_type with
            | TEClass (obj_class, _) -> Some (var_name, obj_class, arg_regions_used)
            | _                      -> None )
          | ObjField _ ->
              (* If passing in a field of an object, the regions required are that of the
                 fields, not the object itself so we don't track. *)
              None)
        possible_expr_reduced_ids
  | None, _ -> []

let use_all_identifier_regions id =
  match id with
  | Variable (var_type, var_name, regions) -> (
    match var_type with
    | TEClass (obj_class, _) -> [(var_name, obj_class, regions)]
    | _                      -> [] )
  | ObjField (obj_class, obj_name, _, _, regions) -> [(obj_name, obj_class, regions)]

let choose_identifier_regions id =
  (* prefer regions with non-linear capabilities, as least restrictive *)
  let choose_regions regions =
    match List.find ~f:(fun (TRegion (cap, _)) -> not (cap = Linear)) regions with
    | Some region -> [region]
    | None        ->
        if
          List.is_empty regions
          (* Don't worry if no region found - this will be caught in later type-checking. *)
        then []
        else [List.hd_exn regions] in
  match id with
  | Variable (var_type, var_name, regions) -> (
    match var_type with
    | TEClass (obj_class, _) -> [(var_name, obj_class, choose_regions regions)]
    | _                      -> [] )
  | ObjField (obj_class, obj_name, _, _, regions) ->
      [(obj_name, obj_class, choose_regions regions)]

let rec collate_region_accesses_expr class_defns function_defns expr =
  let collate_region_accesses_expr_rec =
    collate_region_accesses_expr class_defns function_defns in
  let collate_region_accesses_block_expr_rec =
    collate_region_accesses_block_expr class_defns function_defns in
  match expr with
  | Integer _ | Boolean _ -> (expr, [])
  | Identifier (loc, id) ->
      choose_identifier_regions id
      |> fun region_accesses -> (Identifier (loc, id), region_accesses)
  | BlockExpr (loc, block_expr) ->
      collate_region_accesses_block_expr_rec block_expr
      |> fun (updated_block, region_accesses) ->
      (BlockExpr (loc, updated_block), region_accesses)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      List.unzip
        (List.map
           ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
             collate_region_accesses_expr_rec expr
             |> fun (updated_expr, arg_region_accesses) ->
             (ConstructorArg (type_expr, field_name, updated_expr), arg_region_accesses))
           constructor_args)
      |> fun (updated_args, args_region_accesses) ->
      ( Constructor (loc, type_expr, class_name, updated_args)
      , List.concat args_region_accesses )
  | Let (loc, type_expr, var_name, bound_expr) ->
      collate_region_accesses_expr_rec bound_expr
      |> fun (updated_bound_expr, region_accesses) ->
      (Let (loc, type_expr, var_name, updated_bound_expr), region_accesses)
  | Assign (loc, type_expr, id, assigned_expr) ->
      choose_identifier_regions id
      |> fun id_region_accesses ->
      collate_region_accesses_expr_rec assigned_expr
      |> fun (updated_assigned_expr, expr_region_accesses) ->
      ( Assign (loc, type_expr, id, updated_assigned_expr)
      , id_region_accesses @ expr_region_accesses )
  | Consume (loc, id) ->
      use_all_identifier_regions id
      |> fun id_region_accesses -> (Consume (loc, id), id_region_accesses)
  | MethodApp (loc, type_expr, obj_name, obj_class, method_name, args) ->
      List.unzip
        (List.map
           ~f:(fun (param, arg) ->
             collate_region_accesses_expr_rec arg
             |> fun (updated_arg, arg_region_accesses) ->
             ( updated_arg
             , get_arg_regions_used_by_fn class_defns param arg @ arg_region_accesses ))
           (List.zip_exn (get_method_params obj_class method_name class_defns) args))
      |> fun (updated_args, args_region_accesses) ->
      get_method_effect_regions obj_class method_name class_defns
      |> fun obj_method_regions_used ->
      ( MethodApp (loc, type_expr, obj_name, obj_class, method_name, updated_args)
      , (obj_name, obj_class, obj_method_regions_used) :: List.concat args_region_accesses
      )
  | FunctionApp (loc, return_type, func_name, args) ->
      List.unzip
        (List.map
           ~f:(fun (param, arg) ->
             collate_region_accesses_expr_rec arg
             |> fun (updated_arg, arg_region_accesses) ->
             ( updated_arg
             , get_arg_regions_used_by_fn class_defns param arg @ arg_region_accesses ))
           (List.zip_exn (get_function_params func_name function_defns) args))
      |> fun (updated_args, args_region_accesses) ->
      ( FunctionApp (loc, return_type, func_name, updated_args)
      , List.concat args_region_accesses )
  | Printf (loc, format_str, args) ->
      List.unzip
        (List.map
           ~f:(fun arg ->
             collate_region_accesses_expr_rec arg
             |> fun (updated_arg, arg_region_accesses) ->
             (updated_arg, arg_region_accesses))
           args)
      |> fun (updated_args, args_region_accesses) ->
      (Printf (loc, format_str, updated_args), List.concat args_region_accesses)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      List.unzip
        (List.map
           ~f:(fun (AsyncExpr (free_vars, expr)) ->
             collate_region_accesses_block_expr_rec expr
             |> fun (updated_expr, expr_region_accesses) ->
             List.map
               ~f:(collate_region_accesses_thread_free_var expr_region_accesses)
               free_vars
             |> fun updated_free_vars ->
             (AsyncExpr (updated_free_vars, updated_expr), expr_region_accesses))
           async_exprs)
      |> fun (updated_async_exprs, async_exprs_region_accesses) ->
      collate_region_accesses_block_expr_rec curr_thread_expr
      |> fun (updated_curr_thread_expr, curr_thread_region_accesses) ->
      List.map
        ~f:(collate_region_accesses_thread_free_var curr_thread_region_accesses)
        curr_thread_free_vars
      |> fun updated_curr_thread_free_vars ->
      ( FinishAsync
          ( loc
          , type_expr
          , updated_async_exprs
          , updated_curr_thread_free_vars
          , updated_curr_thread_expr )
      , curr_thread_region_accesses @ List.concat async_exprs_region_accesses )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      collate_region_accesses_expr_rec cond_expr
      |> fun (updated_cond_expr, cond_region_accesses) ->
      collate_region_accesses_block_expr_rec then_expr
      |> fun (updated_then_expr, then_region_accesses) ->
      collate_region_accesses_block_expr_rec else_expr
      |> fun (updated_else_expr, else_region_accesses) ->
      ( If (loc, type_expr, updated_cond_expr, updated_then_expr, updated_else_expr)
      , cond_region_accesses @ then_region_accesses @ else_region_accesses )
  | While (loc, cond_expr, loop_expr) ->
      collate_region_accesses_expr_rec cond_expr
      |> fun (updated_cond_expr, cond_region_accesses) ->
      collate_region_accesses_block_expr_rec loop_expr
      |> fun (updated_loop_expr, loop_region_accesses) ->
      ( While (loc, updated_cond_expr, updated_loop_expr)
      , cond_region_accesses @ loop_region_accesses )
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      collate_region_accesses_expr_rec expr1
      |> fun (updated_expr1, expr1_region_accesses) ->
      collate_region_accesses_expr_rec expr2
      |> fun (updated_expr2, expr2_region_accesses) ->
      ( BinOp (loc, type_expr, binop, updated_expr1, updated_expr2)
      , expr1_region_accesses @ expr2_region_accesses )
  | UnOp (loc, type_expr, unop, expr) ->
      collate_region_accesses_expr_rec expr
      |> fun (updated_expr, expr_region_accesses) ->
      (UnOp (loc, type_expr, unop, updated_expr), expr_region_accesses)

and collate_region_accesses_block_expr class_defns function_defns
    (Block (loc, type_block_expr, exprs)) =
  List.unzip (List.map ~f:(collate_region_accesses_expr class_defns function_defns) exprs)
  |> fun (updated_exprs, region_accesses) ->
  (Block (loc, type_block_expr, updated_exprs), List.concat region_accesses)
