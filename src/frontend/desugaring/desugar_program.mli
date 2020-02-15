(** This module is responsible for desugared the typed AST. The desugared AST simplifies
    the expressions in the typed ASTs It provides the invariant that there is no variable
    shadowing in the desugared AST *)

open Core

val desugar_program : Typing.Typed_ast.program -> Desugared_ast.program Or_error.t
