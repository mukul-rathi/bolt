(** This module replaces the generic class definitions with their instantiations (one for
    each concrete type parameter they're instantiated with) *)

open Ast.Ast_types
open Typing

val replace_generic_with_instantiated_class_defns :
     Typed_ast.class_defn list
  -> (Class_name.t * type_expr list) list
  -> Typed_ast.class_defn list
