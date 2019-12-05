(** Type check parsed expressions given the class and trait definitions - return the
    expression annotated with types if type-checking succeeds. *)

open Ast.Ast_types
open Core

val type_expr :
     class_defn list
  -> trait_defn list
  -> Parsing.Parsed_ast.expr
  -> Typed_ast.expr Or_error.t
