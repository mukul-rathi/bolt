open Ast.Ast_types
open Core
open Ir_gen_operators

let ir_gen_identifier = function
  | Desugaring.Desugared_ast.Variable (_, var_name) ->
      Llvm_ast.Variable (Var_name.to_string var_name)
  | Desugaring.Desugared_ast.ObjField (_, var_name, _, field_name) ->
      Llvm_ast.ObjField (Var_name.to_string var_name, Field_name.to_string field_name)

(* Name mangles method name so doesn't clash with other methods/functionss *)
let ir_gen_method_name meth_name = function
  | TEClass class_name ->
      Ok
        (Fmt.str "_%s_%s"
           (Class_name.to_string class_name)
           (Method_name.to_string meth_name))
  | wrong_type         ->
      Error
        (Error.of_string
           (Fmt.str
              "IR Gen error: can't name mangle method %s as variable is of type %s@."
              (Method_name.to_string meth_name)
              (string_of_type wrong_type)))

let rec ir_gen_expr expr =
  let open Result in
  match expr with
  | Desugaring.Desugared_ast.Unit _ -> Ok Llvm_ast.Unit
  | Desugaring.Desugared_ast.Integer (_, i) -> Ok (Llvm_ast.Integer i)
  | Desugaring.Desugared_ast.Boolean (_, b) -> Ok (Llvm_ast.Boolean b)
  | Desugaring.Desugared_ast.Identifier (_, id) ->
      Ok (Llvm_ast.Identifier (ir_gen_identifier id))
  | Desugaring.Desugared_ast.Constructor (_, _, class_name, constructor_args) ->
      Result.all (List.map ~f:ir_gen_constructor_arg constructor_args)
      >>| fun ir_constructor_args ->
      Llvm_ast.Constructor (Class_name.to_string class_name, ir_constructor_args)
  | Desugaring.Desugared_ast.Let (_, _, var_name, bound_expr) ->
      ir_gen_expr bound_expr
      >>| fun ir_bound_expr -> Llvm_ast.Let (Var_name.to_string var_name, ir_bound_expr)
  | Desugaring.Desugared_ast.Assign (_, _, id, assigned_expr) ->
      ir_gen_expr assigned_expr
      >>| fun ir_assigned_expr -> Llvm_ast.Assign (ir_gen_identifier id, ir_assigned_expr)
  | Desugaring.Desugared_ast.Consume (_, id) ->
      Ok (Llvm_ast.Consume (ir_gen_identifier id))
  | Desugaring.Desugared_ast.MethodApp (_, _, this, obj_type, method_name, args) ->
      ir_gen_method_name method_name obj_type
      >>= fun ir_method_name ->
      Result.all (List.map ~f:ir_gen_expr args)
      >>| fun ir_args ->
      Llvm_ast.FunctionApp
        ( ir_method_name
        , Llvm_ast.Identifier (Llvm_ast.Variable (Var_name.to_string this)) :: ir_args )
  | Desugaring.Desugared_ast.FunctionApp (_, _, func_name, args) ->
      Result.all (List.map ~f:ir_gen_expr args)
      >>| fun ir_args -> Llvm_ast.FunctionApp (Function_name.to_string func_name, ir_args)
  | Desugaring.Desugared_ast.FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      let ir_gen_async_expr async_expr = Result.all (List.map ~f:ir_gen_expr async_expr) in
      Result.all (List.map ~f:ir_gen_async_expr async_exprs)
      >>= fun ir_async_exprs ->
      Result.all (List.map ~f:ir_gen_expr curr_thread_expr)
      >>| fun ir_curr_thread_expr ->
      Llvm_ast.FinishAsync (ir_async_exprs, ir_curr_thread_expr)
  | Desugaring.Desugared_ast.If (_, _, cond_expr, then_expr, else_expr) ->
      ir_gen_expr cond_expr
      >>= fun ir_cond_expr ->
      Result.all (List.map ~f:ir_gen_expr then_expr)
      >>= fun ir_then_expr ->
      Result.all (List.map ~f:ir_gen_expr else_expr)
      >>| fun ir_else_expr -> Llvm_ast.If (ir_cond_expr, ir_then_expr, ir_else_expr)
  | Desugaring.Desugared_ast.While (_, cond_expr, loop_expr) ->
      ir_gen_expr cond_expr
      >>= fun ir_cond_expr ->
      Result.all (List.map ~f:ir_gen_expr loop_expr)
      >>| fun ir_loop_expr -> Llvm_ast.While (ir_cond_expr, ir_loop_expr)
  | Desugaring.Desugared_ast.BinOp (_, _, bin_op, expr1, expr2) ->
      ir_gen_expr expr1
      >>= fun ir_expr1 ->
      ir_gen_expr expr2
      >>| fun ir_expr2 -> Llvm_ast.BinOp (ir_gen_bin_op bin_op, ir_expr1, ir_expr2)
  | Desugaring.Desugared_ast.UnOp (_, _, un_op, expr) ->
      ir_gen_expr expr >>| fun ir_expr -> Llvm_ast.UnOp (ir_gen_un_op un_op, ir_expr)

and ir_gen_constructor_arg (ConstructorArg (_, field_name, expr)) =
  let open Result in
  ir_gen_expr expr
  >>| fun ir_expr -> Llvm_ast.ConstructorArg (Field_name.to_string field_name, ir_expr)
