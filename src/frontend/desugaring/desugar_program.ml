open Core
open Desugar_expr
open Desugar_class_and_function_defns
open Remove_variable_shadowing
open Pprint_dast

let desugar_program (Typing.Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  List.map ~f:(desugar_class_defn class_defns function_defns) class_defns
  |> fun desugared_class_defns ->
  List.map ~f:(desugar_function_defn class_defns function_defns) function_defns
  |> fun desugared_function_defns ->
  desugar_block_expr class_defns function_defns [] main_expr
  |> fun desugared_main_expr ->
  let desugared_program =
    Desugared_ast.Prog
      (desugared_class_defns, desugared_function_defns, desugared_main_expr) in
  remove_var_shadowing_program desugared_program

let pprint_desugared_ast ppf prog = pprint_program ppf prog
