(** This module type-checks overloading in functions and methods. *)

open Core
open Ast.Ast_types

(** We pass in the args types to find the matching overloaded function/method defn. *)

val get_matching_function_type :
     Function_name.t
  -> type_expr list
  -> Parsing.Parsed_ast.function_defn list
  -> loc
  -> (type_expr list * type_expr) Or_error.t

val get_matching_method_type :
     Method_name.t
  -> type_expr list
  -> Parsing.Parsed_ast.class_defn
  -> loc
  -> (type_expr sexp_list * type_expr) Or_error.t
