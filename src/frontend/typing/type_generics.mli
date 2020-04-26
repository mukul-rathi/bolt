(** This module type-checks generic classes (those with type parameters). *)

open Ast.Ast_types
open Core

val instantiate_maybe_generic_this :
  Parsing.Parsed_ast.class_defn -> Var_name.t * type_expr

val instantiate_maybe_generic_class_defn :
     Parsing.Parsed_ast.class_defn
  -> type_expr option (** maybe instantiated with type param *)
  -> loc
  -> Parsing.Parsed_ast.class_defn Or_error.t
