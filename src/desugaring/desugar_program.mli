open Core

val desugar_program : Typing.Typed_ast.program -> Desugared_ast.program Or_error.t

val pprint_desugared_ast : Format.formatter -> Desugared_ast.program -> unit
(** Given a formatter and desugared AST, pretty-print the AST - useful for debugging *)
