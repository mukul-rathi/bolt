(** This module type-checks class inheritance *)

open Core

val type_class_inheritance :
  Parsing.Parsed_ast.class_defn -> Parsing.Parsed_ast.class_defn list -> unit Or_error.t
