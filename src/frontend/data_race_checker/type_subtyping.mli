(** This module type-checks behavioural subtyping - that a class preserves the capability
    behaviour of its superclass. *)

open Core
open Desugaring.Desugared_ast

val type_subtyping : class_defn list -> class_defn -> unit Or_error.t
