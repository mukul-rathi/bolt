open Core

val desugar_expr :
     Typing.Typed_ast.class_defn list
  -> Typing.Typed_ast.expr
  -> Data_race_checker_ast.expr Or_error.t

val desugar_block_expr :
     Typing.Typed_ast.class_defn list
  -> Typing.Typed_ast.block_expr
  -> Data_race_checker_ast.block_expr Or_error.t
