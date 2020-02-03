open Core
open Desugar_expr
open Remove_variable_shadowing

let desugar_function_defn
    (Typing.Typed_ast.TFunction (func_name, ret_type, params, body_expr)) =
  let open Result in
  desugar_block_expr body_expr
  >>| fun desugared_body_expr ->
  Data_race_checker_ast.TFunction (func_name, ret_type, params, desugared_body_expr)

let desugar_method_defn
    (Typing.Typed_ast.TMethod (method_name, ret_type, params, region_effects, body_expr))
    =
  let open Result in
  desugar_block_expr body_expr
  >>| fun desugared_body_expr ->
  Data_race_checker_ast.TMethod
    (method_name, ret_type, params, region_effects, desugared_body_expr)

let desugar_class_defn
    (Typing.Typed_ast.TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  Result.all (List.map ~f:desugar_method_defn method_defns)
  >>| fun desugared_method_defns ->
  Data_race_checker_ast.TClass (class_name, regions, fields, desugared_method_defns)

let desugar_program (Typing.Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all (List.map ~f:desugar_class_defn class_defns)
  >>= fun desugared_class_defns ->
  Result.all (List.map ~f:desugar_function_defn function_defns)
  >>= fun desugared_function_defns ->
  desugar_block_expr main_expr
  >>= fun desugared_main_expr ->
  remove_var_shadowing_program
    (Data_race_checker_ast.Prog
       (desugared_class_defns, desugared_function_defns, desugared_main_expr))

let pprint_data_race_checker_ast ppf (prog : Data_race_checker_ast.program) =
  Pprint_dast.pprint_program ppf prog
