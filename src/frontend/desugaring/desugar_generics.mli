(** This module desugars generic classes into normal classes, before any further
    desugaring occurs *)

open Typing

val desugar_generics_program : Typed_ast.program -> Typed_ast.program
