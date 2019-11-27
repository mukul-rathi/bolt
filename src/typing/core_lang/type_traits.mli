(** Check trait definitions are well-formed *)

open Ast_types
open Core

val type_trait_defns : trait_defn list -> unit Or_error.t
