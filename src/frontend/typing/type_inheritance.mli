(** This module type-checks class inheritance *)

open Core
open Ast.Ast_types
open Parsing

val is_subtype_of : Parsed_ast.class_defn list -> type_expr -> type_expr -> bool

val are_subtypes_of :
  Parsed_ast.class_defn list -> type_expr list -> type_expr list -> bool

val type_class_inheritance :
  Parsed_ast.class_defn -> Parsed_ast.class_defn list -> unit Or_error.t
