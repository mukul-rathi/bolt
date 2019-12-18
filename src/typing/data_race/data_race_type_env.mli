(** This module consists of helper functions for manipulating the environment during the
    data-race type-checking phase. *)

open Ast.Ast_types
open Core
open Typing_core_lang

(** Getter methods used in data-race type-checking *)

val get_function_body_expr :
  Function_name.t -> Typed_ast.function_defn list -> loc -> Typed_ast.expr Or_error.t

val get_method_body_expr :
     Function_name.t
  -> type_expr
  -> Typed_ast.class_defn list
  -> loc
  -> Typed_ast.expr Or_error.t

val get_type_capability :
  type_expr -> Typed_ast.class_defn list -> loc -> capability Or_error.t
