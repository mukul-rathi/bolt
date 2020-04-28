(** This module is used to count the number of different concrete type parameters generic
    classes are instantiated with. *)

open Typing
open Ast.Ast_types

val count_generics_instantiations_program :
  Typed_ast.program -> (Class_name.t * type_expr list) list
