open Core

let desugar_identifier = function
  | Typing.Typed_ast.Variable (var_type, var_name) ->
      Desugared_ast.Variable (var_type, var_name)
  | Typing.Typed_ast.ObjField (obj_type, obj_name, field_type, field_name) ->
      Desugared_ast.ObjField (obj_type, obj_name, field_type, field_name)

let rec desugar_expr expr =
  (* Helper function since desugar_expr recursive call returns a list not a single expr *)
  let open Result in
  let desugar_single_expr expr =
    desugar_expr expr
    >>= function
    | [desugared_expr] -> Ok desugared_expr
    | _                ->
        Error
          (Error.of_string
             (Fmt.str "Error: desugar recursive call expected single expression@."))
    (* Should never be called *) in
  match expr with
  | Typing.Typed_ast.Unit loc -> Ok [Desugared_ast.Unit loc]
  | Typing.Typed_ast.Integer (loc, i) -> Ok [Desugared_ast.Integer (loc, i)]
  | Typing.Typed_ast.Boolean (loc, b) -> Ok [Desugared_ast.Boolean (loc, b)]
  | Typing.Typed_ast.Identifier (loc, id) ->
      Ok [Desugared_ast.Identifier (loc, desugar_identifier id)]
  | Typing.Typed_ast.Block (_, _, exprs) ->
      Result.all (List.map ~f:desugar_expr exprs) >>| List.concat
  | Typing.Typed_ast.Constructor (loc, type_expr, class_name, constructor_args) ->
      (* Each constructor arg has a separate environment, so we don't accumulate var maps *)
      Result.all
        (List.map
           ~f:(fun (Typing.Typed_ast.ConstructorArg (type_expr, field_name, expr)) ->
             desugar_single_expr expr
             >>| fun desugared_expr ->
             Desugared_ast.ConstructorArg (type_expr, field_name, desugared_expr))
           constructor_args)
      >>| fun desugared_constructor_args ->
      [Desugared_ast.Constructor (loc, type_expr, class_name, desugared_constructor_args)]
  | Typing.Typed_ast.Let (loc, type_expr, var_name, bound_expr) ->
      desugar_single_expr bound_expr
      >>| fun desugared_bound_expr ->
      [Desugared_ast.Let (loc, type_expr, var_name, desugared_bound_expr)]
  | Typing.Typed_ast.Assign (loc, type_expr, id, assigned_expr) ->
      desugar_single_expr assigned_expr
      >>| fun desugared_assigned_expr ->
      [ Desugared_ast.Assign
          (loc, type_expr, desugar_identifier id, desugared_assigned_expr) ]
  | Typing.Typed_ast.Consume (loc, id) ->
      Ok [Desugared_ast.Consume (loc, desugar_identifier id)]
  | Typing.Typed_ast.MethodApp (loc, type_expr, var_name, obj_type, method_name, args) ->
      Result.all (List.map ~f:desugar_single_expr args)
      >>| fun desugared_args ->
      [ Desugared_ast.MethodApp
          (loc, type_expr, var_name, obj_type, method_name, desugared_args) ]
  | Typing.Typed_ast.FunctionApp (loc, type_expr, func_name, args) ->
      Result.all (List.map ~f:desugar_single_expr args)
      >>| fun desugared_args ->
      [Desugared_ast.FunctionApp (loc, type_expr, func_name, desugared_args)]
  | Typing.Typed_ast.FinishAsync (loc, type_expr, async_exprs, curr_thread_expr) ->
      Result.all (List.map ~f:desugar_expr async_exprs)
      >>= fun desugared_async_exprs ->
      desugar_expr curr_thread_expr
      >>| fun desugared_curr_thread_expr ->
      [ Desugared_ast.FinishAsync
          (loc, type_expr, desugared_async_exprs, desugared_curr_thread_expr) ]
  | Typing.Typed_ast.If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      desugar_single_expr cond_expr
      >>= fun desugared_cond_expr ->
      desugar_expr then_expr
      >>= fun desugared_then_expr ->
      desugar_expr else_expr
      >>| fun desugared_else_expr ->
      [ Desugared_ast.If
          (loc, type_expr, desugared_cond_expr, desugared_then_expr, desugared_else_expr)
      ]
  | Typing.Typed_ast.While (loc, cond_expr, loop_expr) ->
      desugar_single_expr cond_expr
      >>= fun desugared_cond_expr ->
      desugar_expr loop_expr
      >>| fun desugared_loop_expr ->
      [Desugared_ast.While (loc, desugared_cond_expr, desugared_loop_expr)]
  | Typing.Typed_ast.For (loc, start_expr, cond_expr, step_expr, loop_expr) ->
      desugar_single_expr start_expr
      >>= fun desugared_start_expr ->
      desugar_single_expr cond_expr
      >>= fun desugared_cond_expr ->
      desugar_expr step_expr
      >>= fun desugared_step_expr ->
      desugar_expr loop_expr
      >>| fun desugared_loop_expr ->
      [ desugared_start_expr
      ; Desugared_ast.While
          (loc, desugared_cond_expr, desugared_loop_expr @ desugared_step_expr) ]
  | Typing.Typed_ast.BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      desugar_single_expr expr1
      >>= fun desugared_expr1 ->
      desugar_single_expr expr2
      >>| fun desugared_expr2 ->
      [Desugared_ast.BinOp (loc, type_expr, bin_op, desugared_expr1, desugared_expr2)]
  | Typing.Typed_ast.UnOp (loc, type_expr, un_op, expr) ->
      desugar_single_expr expr
      >>| fun desugared_expr ->
      [Desugared_ast.UnOp (loc, type_expr, un_op, desugared_expr)]
