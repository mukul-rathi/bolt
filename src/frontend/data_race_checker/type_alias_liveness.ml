open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Update_identifier_capabilities

let type_alias_liveness_identifier aliased_obj_name possible_aliases capability_filter_fn
    live_aliases id =
  let id_name = get_identifier_name id in
  (* if we have live aliases, then the object is not linear at this point. *)
  if id_name = aliased_obj_name then
    if List.is_empty live_aliases then (id, live_aliases)
    else
      ( update_identifier_capabilities aliased_obj_name capability_filter_fn id
      , live_aliases )
  else
    ( match
        List.find
          ~f:(fun poss_alias -> identifier_matches_var_name poss_alias id)
          possible_aliases
      with
    | Some alias -> alias :: live_aliases
    | None       -> live_aliases )
    |> fun updated_live_aliases -> (id, updated_live_aliases)

let rec type_alias_liveness_expr aliased_obj_name possible_aliases capability_filter_fn
    live_aliases expr =
  let type_alias_liveness_expr_rec =
    type_alias_liveness_expr aliased_obj_name possible_aliases capability_filter_fn in
  let type_alias_liveness_identifier_rec =
    type_alias_liveness_identifier aliased_obj_name possible_aliases capability_filter_fn
  in
  let type_alias_liveness_block_expr_rec =
    type_alias_liveness_block_expr aliased_obj_name possible_aliases capability_filter_fn
  in
  match expr with
  | Identifier (loc, id) ->
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, updated_live_aliases) ->
      (Identifier (loc, updated_id), updated_live_aliases)
  | Integer _ | Boolean _ -> (expr, live_aliases)
  | BlockExpr (loc, block_expr) ->
      type_alias_liveness_block_expr_rec live_aliases block_expr
      |> fun (updated_block_expr, updated_live_aliases) ->
      (BlockExpr (loc, updated_block_expr), updated_live_aliases)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      (* Note we fold right since we run in reverse program execution order. *)
      List.fold_right ~init:([], live_aliases)
        ~f:
          (fun (ConstructorArg (type_expr, field_name, expr)) (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases expr
          |> fun (updated_expr, updated_acc_live_aliases) ->
          ConstructorArg (type_expr, field_name, updated_expr)
          |> fun updated_arg -> (updated_arg :: acc_args, updated_acc_live_aliases))
        constructor_args
      |> fun (updated_args, updated_live_aliases) ->
      (Constructor (loc, type_expr, class_name, updated_args), updated_live_aliases)
  | Let (loc, type_expr, var_name, bound_expr) ->
      (* remove this var from the set of live aliases *)
      type_alias_liveness_expr_rec
        (List.filter ~f:(fun name -> not (var_name = name)) live_aliases)
        bound_expr
      |> fun (updated_bound_expr, updated_live_aliases) ->
      (Let (loc, type_expr, var_name, updated_bound_expr), updated_live_aliases)
  | Assign (loc, type_expr, id, assigned_expr) ->
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, id_updated_live_aliases) ->
      type_alias_liveness_expr_rec id_updated_live_aliases assigned_expr
      |> fun (updated_assigned_expr, post_assigned_expr_live_aliases) ->
      ( Assign (loc, type_expr, updated_id, updated_assigned_expr)
      , post_assigned_expr_live_aliases )
  | Consume (loc, id) ->
      type_alias_liveness_identifier_rec live_aliases id
      |> fun (updated_id, updated_live_aliases) ->
      (Consume (loc, updated_id), updated_live_aliases)
  | MethodApp (loc, type_expr, obj_name, obj_type, method_name, args) ->
      List.fold_right ~init:([], live_aliases)
        ~f:(fun arg (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases arg
          |> fun (updated_arg, updated_acc_live_aliases) ->
          (updated_arg :: acc_args, updated_acc_live_aliases))
        args
      |> fun (updated_args, updated_live_aliases) ->
      ( MethodApp (loc, type_expr, obj_name, obj_type, method_name, updated_args)
      , updated_live_aliases )
  | FunctionApp (loc, return_type, func_name, args) ->
      List.fold_right ~init:([], live_aliases)
        ~f:(fun arg (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases arg
          |> fun (updated_arg, updated_acc_live_aliases) ->
          (updated_arg :: acc_args, updated_acc_live_aliases))
        args
      |> fun (updated_args, updated_live_aliases) ->
      (FunctionApp (loc, return_type, func_name, updated_args), updated_live_aliases)
  | Printf (loc, format_str, args) ->
      List.fold_right ~init:([], live_aliases)
        ~f:(fun arg (acc_args, acc_live_aliases) ->
          type_alias_liveness_expr_rec acc_live_aliases arg
          |> fun (updated_arg, updated_acc_live_aliases) ->
          (updated_arg :: acc_args, updated_acc_live_aliases))
        args
      |> fun (updated_args, updated_live_aliases) ->
      (Printf (loc, format_str, updated_args), updated_live_aliases)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      (* note the async expressions are forked, so we treat as independent (hence map not
         fold) *)
      List.unzip
        (List.map
           ~f:(fun (AsyncExpr (free_vars, async_expr)) ->
             type_alias_liveness_block_expr_rec live_aliases async_expr
             |> fun (updated_async_expr, updated_async_live_aliases) ->
             (AsyncExpr (free_vars, updated_async_expr), updated_async_live_aliases))
           async_exprs)
      |> fun (updated_async_exprs, async_live_aliases) ->
      type_alias_liveness_block_expr_rec live_aliases curr_thread_expr
      |> fun (updated_curr_thread_expr, curr_thread_live_aliases) ->
      List.concat (curr_thread_live_aliases :: async_live_aliases)
      |> fun updated_live_aliases ->
      ( FinishAsync
          ( loc
          , type_expr
          , updated_async_exprs
          , curr_thread_free_vars
          , updated_curr_thread_expr )
      , updated_live_aliases )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      type_alias_liveness_block_expr_rec live_aliases then_expr
      |> fun (updated_then_expr, then_live_aliases) ->
      type_alias_liveness_block_expr_rec live_aliases else_expr
      |> fun (updated_else_expr, else_live_aliases) ->
      type_alias_liveness_expr_rec (then_live_aliases @ else_live_aliases) cond_expr
      |> fun (updated_cond_expr, cond_live_aliases) ->
      ( If (loc, type_expr, updated_cond_expr, updated_then_expr, updated_else_expr)
      , cond_live_aliases )
  | While (loc, cond_expr, loop_expr) ->
      type_alias_liveness_loop_expr aliased_obj_name possible_aliases capability_filter_fn
        live_aliases loop_expr
      |> fun (updated_loop_expr, loop_live_aliases) ->
      type_alias_liveness_expr_rec loop_live_aliases cond_expr
      |> fun (updated_cond_expr, cond_live_aliases) ->
      (While (loc, updated_cond_expr, updated_loop_expr), cond_live_aliases)
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      (* right-to-left as opposite of program execution order *)
      type_alias_liveness_expr_rec live_aliases expr2
      |> fun (updated_expr2, expr2_live_aliases) ->
      type_alias_liveness_expr_rec expr2_live_aliases expr1
      |> fun (updated_expr_1, expr1_live_aliases) ->
      (BinOp (loc, type_expr, binop, updated_expr_1, updated_expr2), expr1_live_aliases)
  | UnOp (loc, type_expr, unop, expr) ->
      type_alias_liveness_expr_rec live_aliases expr
      |> fun (updated_expr, updated_live_aliases) ->
      (UnOp (loc, type_expr, unop, updated_expr), updated_live_aliases)

and type_alias_liveness_block_expr aliased_obj_name possible_aliases capability_filter_fn
    live_aliases (Block (loc, type_expr, exprs)) =
  let type_alias_liveness_expr_rec =
    type_alias_liveness_expr aliased_obj_name possible_aliases capability_filter_fn in
  List.fold_right ~init:([], live_aliases)
    ~f:(fun expr (acc_exprs, acc_live_aliases) ->
      type_alias_liveness_expr_rec acc_live_aliases expr
      |> fun (updated_expr, updated_acc_live_aliases) ->
      (updated_expr :: acc_exprs, updated_acc_live_aliases))
    exprs
  |> fun (updated_exprs, updated_live_aliases) ->
  (Block (loc, type_expr, updated_exprs), updated_live_aliases)

(* compute least fixed point of loop liveness aliases *)
and type_alias_liveness_loop_expr aliased_obj_name possible_aliases capability_filter_fn
    live_aliases loop_expr =
  type_alias_liveness_block_expr aliased_obj_name possible_aliases capability_filter_fn
    live_aliases loop_expr
  |> fun (updated_loop_expr, updated_live_aliases) ->
  if var_lists_are_equal live_aliases updated_live_aliases then
    (updated_loop_expr, updated_live_aliases)
  else
    type_alias_liveness_loop_expr aliased_obj_name possible_aliases capability_filter_fn
      updated_live_aliases updated_loop_expr
