open Core
open Desugar_expr
open Desugar_class_and_function_defns

let desugar_program (Typing.Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all (List.map ~f:desugar_class_defn class_defns)
  >>= fun desugared_class_defns ->
  Result.all (List.map ~f:desugar_function_defn function_defns)
  >>= fun desugared_function_defns ->
  desugar_expr main_expr
  >>| fun desugared_main_expr ->
  Desugared_ast.Prog (desugared_class_defns, desugared_function_defns, desugared_main_expr)

let pprint_desugared_ast ppf (prog : Desugared_ast.program) =
  Pprint_dast.pprint_program ppf prog
