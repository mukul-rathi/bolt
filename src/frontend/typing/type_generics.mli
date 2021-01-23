(** This module type-checks the usage of generics. *)

open Ast.Ast_types
open Core
open Parsing

(** Type usage of generic types *)

val type_generics_usage_main_expr : Parsed_ast.block_expr -> unit Or_error.t
val type_generics_usage_function_defn : Parsed_ast.function_defn -> unit Or_error.t
val type_generics_usage_class_defn : Parsed_ast.class_defn -> unit Or_error.t

(** Instantiate generic types *)

val instantiate_maybe_generic_this : Parsed_ast.class_defn -> Type_env.type_binding
(** Instantiate "this" - to be used when type-checking a class *)

val instantiate_maybe_generic_class_defn :
     type_expr option (** maybe instantiated with type param *)
  -> Parsed_ast.class_defn
  -> loc
  -> Parsed_ast.class_defn Or_error.t
(** Returns the class definition replacing any generic type parameters with the concrete
    type passed in. *)
