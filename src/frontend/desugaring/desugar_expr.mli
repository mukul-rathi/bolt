open Core

val desugar_expr : Typing.Typed_ast.expr -> Desugared_ast.expr list Or_error.t
(** Given a typed AST expr, desugar it - note the expression may be desugared into a
    sequence of simpler expressions (hence the [list] return type) *)
