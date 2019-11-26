(** This module checks linear references are not aliased / owned by multiple aliases. *)

open Core
open Typed_ast
open Ast_types

val type_linear_ownership : class_defn list -> trait_defn list -> expr -> unit Or_error.t
