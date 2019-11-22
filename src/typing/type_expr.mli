open Ast_types
open Core

val type_expr :
     class_defn list
  -> trait_defn list
  -> Parsed_ast.expr
  -> (Typed_ast.expr, Error.t) result
(** Type check expressions *)
