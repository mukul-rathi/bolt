open Core

val desugar_expr : Typing.Typed_ast.expr -> Desugared_ast.expr list Or_error.t
