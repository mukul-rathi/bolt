(** This module type-checks overloading in functions and methods. *)

open Core
open Ast.Ast_types

val type_overloaded_function_defns :
  Parsing.Parsed_ast.function_defn list -> unit Or_error.t
(** Check functions are overloaded correctly (i.e. each has a different number/type of
    args) *)

val type_overloaded_method_defns : Parsing.Parsed_ast.method_defn list -> unit Or_error.t

val get_matching_function_type :
     Function_name.t
  -> type_expr list
  -> Parsing.Parsed_ast.function_defn list
  -> loc
  -> (type_expr list * type_expr) Or_error.t
(** We pass in the args types to find the matching overloaded function/method defn. *)

val get_matching_method_type :
     Parsing.Parsed_ast.class_defn list
  -> Method_name.t
  -> type_expr list
  -> Parsing.Parsed_ast.class_defn
  -> type_expr option
  -> loc
  -> (type_expr sexp_list * type_expr) Or_error.t
