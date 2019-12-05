(** This module checks linear references are not aliased / owned by multiple aliases. *)

open Core
open Typing_core_lang.Typed_ast
open Ast.Ast_types

val type_linear_ownership : class_defn list -> trait_defn list -> expr -> unit Or_error.t
