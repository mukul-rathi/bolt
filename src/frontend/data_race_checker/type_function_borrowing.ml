open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types
open Type_linear_capabilities

(* If a param is linear and not borrowed, then the arg_expr cannot reduce to an
   identifier. *)
let check_arg_borrowing class_defns loc ((TParam (param_type, _, _) as param), arg_expr) =
  match param_type with
  | TEClass (param_class, Owned) ->
      let _, _, param_capabilities =
        List.unzip3 (params_to_obj_vars_and_capabilities class_defns [param]) in
      let is_param_linear =
        List.exists
          ~f:(fun capability ->
            capability_fields_have_mode capability param_class Linear class_defns)
          (List.concat param_capabilities) in
      if is_param_linear then
        if List.is_empty (reduce_expr_to_obj_id arg_expr) then Ok ()
        else
          Error
            (Error.of_string
               (Fmt.str "%s Linear non-borrowed argument should be consumed@."
                  (string_of_loc loc)))
      else Ok ()
  | _                            -> Ok ()

let rec type_function_forward_borrowing_expr class_defns function_defns expr =
  let open Result in
  match expr with
  | MethodApp (loc, _, obj_name, obj_class, meth_name, args) ->
      let args_ids = List.concat_map ~f:reduce_expr_to_obj_id args in
      type_linear_obj_method_args class_defns obj_name obj_class args_ids loc
      >>= fun () ->
      type_linear_args class_defns args_ids loc
      >>= fun () ->
      let params = get_method_params obj_class meth_name class_defns in
      Result.all_unit
        (List.map ~f:(check_arg_borrowing class_defns loc) (List.zip_exn params args))
      >>= fun () ->
      (* Recurse on arguments *)
      Result.all_unit
        (List.map
           ~f:(type_function_forward_borrowing_expr class_defns function_defns)
           args)
  | FunctionApp (loc, _, func_name, args) ->
      let args_ids = List.concat_map ~f:reduce_expr_to_obj_id args in
      type_linear_args class_defns args_ids loc
      >>= fun () ->
      let params = get_function_params func_name function_defns in
      Result.all_unit
        (List.map ~f:(check_arg_borrowing class_defns loc) (List.zip_exn params args))
      >>= fun () ->
      (* Recurse on arguments *)
      Result.all_unit
        (List.map
           ~f:(type_function_forward_borrowing_expr class_defns function_defns)
           args)
  (* other cases are just recursion *)
  | Integer _ | Boolean _ -> Ok ()
  | Identifier _ -> Ok ()
  | BlockExpr (_, block_expr) ->
      (type_function_forward_borrowing_block_expr class_defns function_defns) block_expr
  | Constructor (_, _, _, constructor_args) ->
      Result.all_unit
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) ->
             type_function_forward_borrowing_expr class_defns function_defns expr)
           constructor_args)
  | Let (_, _, _, bound_expr) ->
      type_function_forward_borrowing_expr class_defns function_defns bound_expr
  | Assign (_, _, _, assigned_expr) ->
      type_function_forward_borrowing_expr class_defns function_defns assigned_expr
  | Consume (_, _) -> Ok ()
  | Printf (_, _, args) ->
      Result.all_unit
        (List.map
           ~f:(type_function_forward_borrowing_expr class_defns function_defns)
           args)
  | FinishAsync (_, _, async_exprs, _, curr_thread_expr) ->
      Result.all_unit
        (List.map
           ~f:(fun (AsyncExpr (_, expr)) ->
             (type_function_forward_borrowing_block_expr class_defns function_defns) expr)
           async_exprs)
      >>= fun () ->
      (type_function_forward_borrowing_block_expr class_defns function_defns)
        curr_thread_expr
  | If (_, _, cond_expr, then_expr, else_expr) ->
      type_function_forward_borrowing_expr class_defns function_defns cond_expr
      >>= fun () ->
      (type_function_forward_borrowing_block_expr class_defns function_defns) then_expr
      >>= fun () ->
      (type_function_forward_borrowing_block_expr class_defns function_defns) else_expr
  | While (_, cond_expr, loop_expr) ->
      type_function_forward_borrowing_expr class_defns function_defns cond_expr
      >>= fun () ->
      (type_function_forward_borrowing_block_expr class_defns function_defns) loop_expr
  | BinOp (_, _, _, expr1, expr2) ->
      type_function_forward_borrowing_expr class_defns function_defns expr1
      >>= fun () -> type_function_forward_borrowing_expr class_defns function_defns expr2
  | UnOp (_, _, _, expr) ->
      type_function_forward_borrowing_expr class_defns function_defns expr

and type_function_forward_borrowing_block_expr class_defns function_defns
    (Block (_, _, exprs)) =
  Result.all_unit
    (List.map ~f:(type_function_forward_borrowing_expr class_defns function_defns) exprs)

let type_function_reverse_borrowing class_defns error_prefix return_type body_expr =
  match return_type with
  | TEClass (_, Borrowed)       -> Ok () (* if borrowed then fine *)
  | TEClass (class_name, Owned) ->
      if class_has_mode class_name Linear class_defns then
        match reduce_block_expr_to_obj_id body_expr with
        | [] -> Ok ()
        | _  ->
            Error
              (Error.of_string
                 (Fmt.str
                    "%s Body expression may return a borrowed type, which is not allowed.@."
                    error_prefix))
      else Ok () (* if not linear we are not worried about borrowing *)
  | _                           ->
      (* we don't check borrowing for primitive return type *)
      Ok ()
