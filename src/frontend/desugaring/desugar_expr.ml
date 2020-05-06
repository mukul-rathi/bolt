open Core
open Free_obj_vars_expr
open Ast.Ast_types
open Desugar_env
open Desugar_overloading

let dedup_free_vars free_vars =
  List.dedup_and_sort
    ~compare:(fun (var_name_1, _, _) (var_name_2, _, _) ->
      if var_name_1 = var_name_2 then 0 else 1)
    free_vars

(* We use the list of borrowed vars to determine if an identifier has a borrowed
   capability *)
let desugar_identifier class_defns borrowed_vars id =
  match id with
  | Typing.Typed_ast.Variable (var_type, var_name) ->
      let capabilities =
        match var_type with
        | TEClass (class_name, _) -> get_class_capabilities class_name class_defns
        | _                       -> [] in
      let isBorrowed =
        if elem_in_list var_name borrowed_vars then Some Borrowed else None in
      Desugared_ast.Variable (var_type, var_name, capabilities, isBorrowed)
  | Typing.Typed_ast.ObjField (class_name, _, obj_name, field_type, field_name) ->
      get_class_field_capabilities class_name field_name class_defns
      |> fun capabilities ->
      let isBorrowed =
        if elem_in_list obj_name borrowed_vars then Some Borrowed else None in
      Desugared_ast.ObjField
        (class_name, obj_name, field_type, field_name, capabilities, isBorrowed)

let rec desugar_expr class_defns function_defns borrowed_vars expr =
  (* Helper function since desugar_expr recursive call returns a list not a single expr *)
  match expr with
  | Typing.Typed_ast.Integer (loc, i) -> Desugared_ast.Integer (loc, i)
  | Typing.Typed_ast.Boolean (loc, b) -> Desugared_ast.Boolean (loc, b)
  | Typing.Typed_ast.Identifier (loc, id) ->
      desugar_identifier class_defns borrowed_vars id
      |> fun desugared_id -> Desugared_ast.Identifier (loc, desugared_id)
  | Typing.Typed_ast.BlockExpr (loc, block_expr) ->
      desugar_block_expr class_defns function_defns borrowed_vars block_expr
      |> fun desugared_block_expr -> Desugared_ast.BlockExpr (loc, desugared_block_expr)
  | Typing.Typed_ast.Constructor (loc, class_name, _, constructor_args) ->
      List.map
        ~f:(fun (Typing.Typed_ast.ConstructorArg (type_expr, field_name, expr)) ->
          desugar_expr class_defns function_defns borrowed_vars expr
          |> fun desugared_expr ->
          Desugared_ast.ConstructorArg (type_expr, field_name, desugared_expr))
        constructor_args
      |> fun desugared_constructor_args ->
      Desugared_ast.Constructor
        (loc, TEClass (class_name, None), class_name, desugared_constructor_args)
  | Typing.Typed_ast.Let (loc, type_expr, var_name, bound_expr) ->
      desugar_expr class_defns function_defns borrowed_vars bound_expr
      |> fun desugared_bound_expr ->
      Desugared_ast.Let (loc, type_expr, var_name, desugared_bound_expr)
  | Typing.Typed_ast.Assign (loc, type_expr, id, assigned_expr) ->
      desugar_expr class_defns function_defns borrowed_vars assigned_expr
      |> fun desugared_assigned_expr ->
      desugar_identifier class_defns borrowed_vars id
      |> fun desugared_id ->
      Desugared_ast.Assign (loc, type_expr, desugared_id, desugared_assigned_expr)
  | Typing.Typed_ast.Consume (loc, id) ->
      desugar_identifier class_defns borrowed_vars id
      |> fun desugared_id -> Desugared_ast.Consume (loc, desugared_id)
  | Typing.Typed_ast.MethodApp
      (loc, type_expr, method_params, obj_name, obj_class, _, method_name, args) ->
      List.map ~f:(desugar_expr class_defns function_defns borrowed_vars) args
      |> fun desugared_args ->
      get_class_capabilities obj_class class_defns
      |> fun obj_capabilities ->
      Desugared_ast.MethodApp
        ( loc
        , type_expr
        , obj_name
        , obj_capabilities
        , obj_class
        , name_mangle_overloaded_method method_name method_params
        , desugared_args )
  | Typing.Typed_ast.FunctionApp (loc, type_expr, func_params, func_name, args) ->
      List.map ~f:(desugar_expr class_defns function_defns borrowed_vars) args
      |> fun desugared_args ->
      Desugared_ast.FunctionApp
        ( loc
        , type_expr
        , name_mangle_if_overloaded_function function_defns func_name func_params
        , desugared_args )
  | Typing.Typed_ast.Printf (loc, format_str, args) ->
      List.map ~f:(desugar_expr class_defns function_defns borrowed_vars) args
      |> fun desugared_args -> Desugared_ast.Printf (loc, format_str, desugared_args)
  | Typing.Typed_ast.FinishAsync (loc, type_expr, async_exprs, curr_thread_expr) ->
      let free_obj_vars_curr_thread_expr =
        dedup_free_vars (free_obj_vars_block_expr class_defns curr_thread_expr) in
      List.map
        ~f:(desugar_async_expr class_defns function_defns borrowed_vars)
        async_exprs
      |> fun desugared_async_exprs ->
      desugar_block_expr class_defns function_defns borrowed_vars curr_thread_expr
      |> fun desugared_curr_thread_expr ->
      Desugared_ast.FinishAsync
        ( loc
        , type_expr
        , desugared_async_exprs
        , free_obj_vars_curr_thread_expr
        , desugared_curr_thread_expr )
  | Typing.Typed_ast.If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      desugar_expr class_defns function_defns borrowed_vars cond_expr
      |> fun desugared_cond_expr ->
      desugar_block_expr class_defns function_defns borrowed_vars then_expr
      |> fun desugared_then_expr ->
      desugar_block_expr class_defns function_defns borrowed_vars else_expr
      |> fun desugared_else_expr ->
      Desugared_ast.If
        (loc, type_expr, desugared_cond_expr, desugared_then_expr, desugared_else_expr)
  | Typing.Typed_ast.While (loc, cond_expr, loop_expr) ->
      desugar_expr class_defns function_defns borrowed_vars cond_expr
      |> fun desugared_cond_expr ->
      desugar_block_expr class_defns function_defns borrowed_vars loop_expr
      |> fun desugared_loop_expr ->
      Desugared_ast.While (loc, desugared_cond_expr, desugared_loop_expr)
  | Typing.Typed_ast.BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      desugar_expr class_defns function_defns borrowed_vars expr1
      |> fun desugared_expr1 ->
      desugar_expr class_defns function_defns borrowed_vars expr2
      |> fun desugared_expr2 ->
      Desugared_ast.BinOp (loc, type_expr, bin_op, desugared_expr1, desugared_expr2)
  | Typing.Typed_ast.UnOp (loc, type_expr, un_op, expr) ->
      desugar_expr class_defns function_defns borrowed_vars expr
      |> fun desugared_expr -> Desugared_ast.UnOp (loc, type_expr, un_op, desugared_expr)

and desugar_block_expr class_defns function_defns borrowed_vars
    (Typing.Typed_ast.Block (loc, type_expr, exprs)) =
  List.map ~f:(desugar_expr class_defns function_defns borrowed_vars) exprs
  |> fun desugared_exprs -> Desugared_ast.Block (loc, type_expr, desugared_exprs)

and desugar_async_expr class_defns function_defns borrowed_vars
    (Typing.Typed_ast.AsyncExpr async_block_expr) =
  let free_obj_vars_expr =
    dedup_free_vars (free_obj_vars_block_expr class_defns async_block_expr) in
  desugar_block_expr class_defns function_defns borrowed_vars async_block_expr
  |> fun desugared_async_block_expr ->
  Desugared_ast.AsyncExpr (free_obj_vars_expr, desugared_async_block_expr)
