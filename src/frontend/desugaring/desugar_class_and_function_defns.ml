open Core
open Desugar_expr

let desugar_function_defn class_defns
    (Typing.Typed_ast.TFunction (func_name, ret_type, params, body_expr)) =
  let open Result in
  desugar_block_expr class_defns body_expr
  >>| fun desugared_body_expr ->
  Desugared_ast.TFunction (func_name, ret_type, params, desugared_body_expr)

let desugar_method_defn class_defns
    (Typing.Typed_ast.TMethod (method_name, ret_type, params, region_effects, body_expr))
    =
  let open Result in
  desugar_block_expr class_defns body_expr
  >>| fun desugared_body_expr ->
  Desugared_ast.TMethod
    (method_name, ret_type, params, region_effects, desugared_body_expr)

let desugar_class_defn class_defns
    (Typing.Typed_ast.TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  Result.all (List.map ~f:(desugar_method_defn class_defns) method_defns)
  >>| fun desugared_method_defns ->
  Desugared_ast.TClass (class_name, regions, fields, desugared_method_defns)
