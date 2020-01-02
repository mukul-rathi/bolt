(** This module consists of helper functions for manipulating the type environment during
    the core-lang type-checking phase. *)

open Ast.Ast_types
open Core

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

(** A bunch of getter methods used in type-checking the core language *)

val get_var_type : Var_name.t -> type_env -> loc -> type_expr Or_error.t

val get_class_field :
  Field_name.t -> Parsing.Parsed_ast.class_defn -> loc -> field_defn Or_error.t

val get_obj_class_defn :
     Var_name.t
  -> type_env
  -> Parsing.Parsed_ast.class_defn list
  -> loc
  -> Parsing.Parsed_ast.class_defn Or_error.t

val get_class_defn :
     Class_name.t
  -> Parsing.Parsed_ast.class_defn list
  -> loc
  -> Parsing.Parsed_ast.class_defn Or_error.t

val get_class_regions :
  Class_name.t -> Parsing.Parsed_ast.class_defn list -> region list Or_error.t

val get_function_type :
     Function_name.t
  -> Parsing.Parsed_ast.function_defn list
  -> loc
  -> (type_expr list * type_expr) Or_error.t

val get_method_type :
     Method_name.t
  -> Parsing.Parsed_ast.class_defn
  -> loc
  -> (type_expr sexp_list * type_expr) Or_error.t

(** Checker methods - check invariants *)

val check_no_var_shadowing_in_block :
  Parsing.Parsed_ast.expr list -> loc -> unit Or_error.t

val check_identifier_assignable :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.identifier
  -> type_env
  -> loc
  -> unit Or_error.t

val check_identifier_consumable : Parsing.Parsed_ast.identifier -> loc -> unit Or_error.t
val check_variable_declarable : Var_name.t -> loc -> unit Or_error.t
