open Core
open Remove_variable_shadowing
open Ast.Ast_types
open Desugar_expr

let init_var_map_from_params params =
  List.map ~f:(function TParam (_, var, _) -> (var, var)) params

let desugar_function_defn
    (Typing.Typed_ast.TFunction (func_name, ret_type, params, body_expr)) =
  let open Result in
  let param_vars = init_var_map_from_params params in
  remove_var_shadowing body_expr param_vars
  >>= fun (deshadowed_body_expr, _) ->
  desugar_expr deshadowed_body_expr
  >>| fun desugared_body_expr ->
  Desugared_ast.TFunction (func_name, ret_type, params, desugared_body_expr)

let desugar_method_defn
    (Typing.Typed_ast.TMethod (method_name, ret_type, params, region_effects, body_expr))
    =
  let open Result in
  let this = Var_name.of_string "this" in
  let param_vars = (this, this) :: init_var_map_from_params params in
  remove_var_shadowing body_expr param_vars
  >>= fun (deshadowed_body_expr, _) ->
  desugar_expr deshadowed_body_expr
  >>| fun desugared_body_expr ->
  Desugared_ast.TMethod
    (method_name, ret_type, params, region_effects, desugared_body_expr)

let desugar_class_defn
    (Typing.Typed_ast.TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  Result.all (List.map ~f:desugar_method_defn method_defns)
  >>| fun desugared_method_defns ->
  Desugared_ast.TClass (class_name, regions, fields, desugared_method_defns)
