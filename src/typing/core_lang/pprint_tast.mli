(** This module contains functions to print the typed AST (tAST) *)

open Typed_ast

val pprint_program : Format.formatter -> program -> unit
