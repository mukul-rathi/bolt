open Core
open Data_race_checker_ast

(* Used when tracking aliasing *)
let rec reduce_expr_to_id expr =
  match expr with
  | Integer _ | Boolean _ -> None
  | Identifier (_, id) -> Some [id]
  | BlockExpr (_, block_expr) -> reduce_block_expr_to_id block_expr
  | Constructor (_, _, _, _) -> None
  | Let (_, _, _, bound_expr) -> reduce_expr_to_id bound_expr
  | Assign (_, _, _, assigned_expr) -> reduce_expr_to_id assigned_expr
  | Consume (_, id) -> Some [id]
  | MethodApp (_, _, _, _, _, _) -> None
  | FunctionApp (_, _, _, _) -> None
  | Printf (_, _, _) -> None
  | FinishAsync (_, _, _, _, curr_thread_expr) -> reduce_block_expr_to_id curr_thread_expr
  | If (_, _, _, then_expr, else_expr) -> (
      let then_maybe_id = reduce_block_expr_to_id then_expr in
      let else_maybe_id = reduce_block_expr_to_id else_expr in
      match then_maybe_id with
      | None         -> else_maybe_id
      | Some then_id -> (
        match else_maybe_id with
        | None         -> Some then_id
        | Some else_id -> Some (then_id @ else_id) ) )
  | While _ -> None
  | BinOp _ -> None
  | UnOp _ -> None

and reduce_block_expr_to_id (Block (loc, type_expr, exprs)) =
  match exprs with
  | []             -> None
  | [expr]         -> reduce_expr_to_id expr
  | _ :: rem_exprs -> reduce_block_expr_to_id (Block (loc, type_expr, rem_exprs))

(* Check if the expression is reduced to an id that matches a given var_name *)
let var_in_expr_reduced_ids var_name optional_ids =
  match optional_ids with
  | Some ids ->
      List.length
        (List.filter
           ~f:(function
             | Variable (_, name, _, _) -> var_name = name
             | ObjField (_, obj_name, _, _, _, _) -> var_name = obj_name)
           ids)
      > 0
  | None     -> false

let rec find_aliases_in_block_expr var_name (Block (loc, type_block_expr, exprs)) =
  match exprs with
  | []                      -> []
  | expr :: remaining_exprs ->
      let expr_aliases =
        match expr with
        | Let (_, _, name, bound_expr) ->
            if var_in_expr_reduced_ids var_name (reduce_expr_to_id bound_expr) then [name]
            else []
        | _                            -> [] in
      let other_exprs_aliases =
        find_aliases_in_block_expr var_name
          (Block (loc, type_block_expr, remaining_exprs)) in
      List.concat [expr_aliases; other_exprs_aliases]

let update_var_capabilities_identifier var_name caps_update_fn = function
  | Variable (var_type, name, regions, caps) ->
      if var_name = name then caps_update_fn caps ;
      Variable (var_type, name, regions, caps)
  | ObjField (obj_class, obj_name, field_type, field_name, regions, caps) ->
      if var_name = obj_name then caps_update_fn caps ;
      ObjField (obj_class, obj_name, field_type, field_name, regions, caps)

let rec update_var_capabilities_expr var_name caps_update_fn expr =
  let update_var_caps_expr_rec = update_var_capabilities_expr var_name caps_update_fn in
  let update_var_caps_block_expr_rec =
    update_var_capabilities_block_expr var_name caps_update_fn in
  let update_var_caps_identifier_rec =
    update_var_capabilities_identifier var_name caps_update_fn in
  match expr with
  | Integer _ | Boolean _ -> expr
  | Identifier (loc, id) -> Identifier (loc, update_var_caps_identifier_rec id)
  | BlockExpr (loc, block_expr) ->
      BlockExpr (loc, update_var_caps_block_expr_rec block_expr)
  | Constructor (loc, type_expr, class_name, constructor_args) ->
      let updated_args =
        List.map
          ~f:(fun (ConstructorArg (type_expr, field_name, expr)) ->
            ConstructorArg (type_expr, field_name, update_var_caps_expr_rec expr))
          constructor_args in
      Constructor (loc, type_expr, class_name, updated_args)
  | Let (loc, type_expr, var_name, bound_expr) ->
      Let (loc, type_expr, var_name, update_var_caps_expr_rec bound_expr)
  | Assign (loc, type_expr, id, assigned_expr) ->
      Assign
        ( loc
        , type_expr
        , update_var_caps_identifier_rec id
        , update_var_caps_expr_rec assigned_expr )
  | Consume (loc, id) -> Consume (loc, update_var_caps_identifier_rec id)
  | MethodApp (loc, type_expr, obj_name, obj_type, method_name, args) ->
      MethodApp
        ( loc
        , type_expr
        , obj_name
        , obj_type
        , method_name
        , List.map ~f:update_var_caps_expr_rec args )
  | FunctionApp (loc, return_type, func_name, args) ->
      FunctionApp (loc, return_type, func_name, List.map ~f:update_var_caps_expr_rec args)
  | Printf (loc, format_str, args) ->
      Printf (loc, format_str, List.map ~f:update_var_caps_expr_rec args)
  | FinishAsync (loc, type_expr, async_exprs, curr_thread_free_vars, curr_thread_expr) ->
      FinishAsync
        ( loc
        , type_expr
        , List.map
            ~f:(fun (AsyncExpr (free_vars, expr)) ->
              AsyncExpr (free_vars, update_var_caps_block_expr_rec expr))
            async_exprs
        , curr_thread_free_vars
        , update_var_caps_block_expr_rec curr_thread_expr )
  | If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      If
        ( loc
        , type_expr
        , update_var_caps_expr_rec cond_expr
        , update_var_caps_block_expr_rec then_expr
        , update_var_caps_block_expr_rec else_expr )
  | While (loc, cond_expr, loop_expr) ->
      While
        (loc, update_var_caps_expr_rec cond_expr, update_var_caps_block_expr_rec loop_expr)
  | BinOp (loc, type_expr, binop, expr1, expr2) ->
      BinOp
        ( loc
        , type_expr
        , binop
        , update_var_caps_expr_rec expr1
        , update_var_caps_expr_rec expr2 )
  | UnOp (loc, type_expr, unop, expr) ->
      UnOp (loc, type_expr, unop, update_var_caps_expr_rec expr)

and update_var_capabilities_block_expr var_name caps_update_fn
    (Block (loc, type_block_expr, exprs)) =
  let updated_exprs =
    List.map ~f:(update_var_capabilities_expr var_name caps_update_fn) exprs in
  let updated_block_expr = Block (loc, type_block_expr, updated_exprs) in
  List.fold ~init:updated_block_expr
    ~f:(fun acc_block_expr alias ->
      update_var_capabilities_block_expr alias caps_update_fn acc_block_expr)
    (find_aliases_in_block_expr var_name updated_block_expr)
