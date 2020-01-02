open Core
open Remove_variable_shadowing
open Ast.Ast_types
open Desugar_expr

let init_var_map_from_params params =
  List.concat_map ~f:(function TVoid -> [] | TParam (_, var, _) -> [(var, var)]) params

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

let desugar_program (Typing.Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all (List.map ~f:desugar_class_defn class_defns)
  >>= fun desugared_class_defns ->
  Result.all (List.map ~f:desugar_function_defn function_defns)
  >>= fun desugared_function_defns ->
  remove_var_shadowing main_expr []
  >>= fun (deshadowed_main_expr, _) ->
  desugar_expr deshadowed_main_expr
  >>| fun desugared_main_expr ->
  Desugared_ast.Prog (desugared_class_defns, desugared_function_defns, desugared_main_expr)

let pprint_desugared_ast ppf (prog : Desugared_ast.program) =
  Pprint_dast.pprint_program ppf prog
