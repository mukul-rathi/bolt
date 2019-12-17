(** This module checks linear references are not aliased / owned by multiple aliases. *)

open Core
open Typing_core_lang.Typed_ast

val type_linear_ownership : program -> unit Or_error.t
