(** This module type-checks generic classes (those with type parameters). *)

open Ast.Ast_types
open Core
open Parsing

val type_generics_usage_block_expr :
  Parsed_ast.block_expr -> generic_type option -> unit Or_error.t

val type_generics_usage_function_defn : Parsed_ast.function_defn -> unit Or_error.t
val type_generics_usage_class_defn : Parsed_ast.class_defn -> unit Or_error.t
val instantiate_maybe_generic_this : Parsed_ast.class_defn -> Type_env.type_binding

val instantiate_maybe_generic_class_defn :
     type_expr option (** maybe instantiated with type param *)
  -> Parsed_ast.class_defn
  -> loc
  -> Parsed_ast.class_defn Or_error.t
