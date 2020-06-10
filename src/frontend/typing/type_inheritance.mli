(** This module type-checks class inheritance *)

open Core
open Ast.Ast_types

val is_subtype_of : Parsing.Parsed_ast.class_defn list -> type_expr -> type_expr -> bool

val are_subtypes_of :
  Parsing.Parsed_ast.class_defn list -> type_expr list -> type_expr list -> bool

val type_class_inheritance :
  Parsing.Parsed_ast.class_defn -> Parsing.Parsed_ast.class_defn list -> unit Or_error.t
