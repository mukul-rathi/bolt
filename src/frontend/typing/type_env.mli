(** This module consists of helper functions for manipulating the type environment during
    the core language type-checking phase. *)

open Ast.Ast_types
open Core

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

(** A bunch of getter methods used in type-checking the core language *)

val get_var_type : Var_name.t -> type_env -> loc -> type_expr Or_error.t

val get_class_field :
     Field_name.t
  -> Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.class_defn
  -> type_expr option
  -> loc
  -> field_defn Or_error.t

val get_class_methods :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.class_defn
  -> type_expr option
  -> loc
  -> Parsing.Parsed_ast.method_defn list Or_error.t

val get_obj_class_defn :
     Var_name.t
  -> type_env
  -> Parsing.Parsed_ast.class_defn list
  -> loc
  -> (Parsing.Parsed_ast.class_defn * type_expr option) Or_error.t
(** returns class, with any type parameters instantiated *)

val get_class_defn :
     Class_name.t
  -> Parsing.Parsed_ast.class_defn list
  -> loc
  -> Parsing.Parsed_ast.class_defn Or_error.t

val get_instantiated_class_defn :
     Class_name.t
  -> Parsing.Parsed_ast.class_defn list
  -> type_expr option
  -> loc
  -> Parsing.Parsed_ast.class_defn Or_error.t

val get_class_capabilities :
  Class_name.t -> Parsing.Parsed_ast.class_defn list -> capability list Or_error.t

val get_method_capability_annotations :
  Class_name.t -> capability list -> Capability_name.t list -> capability list Or_error.t

(** Checker methods - check invariants *)

val check_no_duplicate_var_declarations_in_block :
  Parsing.Parsed_ast.expr list -> loc -> unit Or_error.t

val check_identifier_assignable :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.identifier
  -> type_env
  -> loc
  -> unit Or_error.t

val check_identifier_consumable :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.identifier
  -> type_env
  -> loc
  -> unit Or_error.t

val check_variable_declarable : Var_name.t -> loc -> unit Or_error.t
