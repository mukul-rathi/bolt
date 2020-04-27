(** This module desugars generic classes into normal classes, before any further
    desugaring occurs *)

open Ast.Ast_types

val name_mangle_if_generic_class : Class_name.t -> type_expr option -> Class_name.t
val desugar_generics_program : Typing.Typed_ast.program -> Typing.Typed_ast.program
