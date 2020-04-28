open Typing
(** This module is responsible for name-mangling generics, so Foo<int> -> _Fooint *)

open Ast.Ast_types

val name_mangle_generic_class : Class_name.t -> type_expr -> Class_name.t
val name_mangle_if_generic_class : Class_name.t -> type_expr option -> Class_name.t
val name_mangle_generics_program : Typed_ast.program -> Typed_ast.program
