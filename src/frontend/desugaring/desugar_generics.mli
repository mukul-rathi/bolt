(** This module desugars generic classes into normal classes, before any further
    desugaring occurs *)

val desugar_generics_program : Typing.Typed_ast.program -> Typing.Typed_ast.program
