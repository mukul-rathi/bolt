(** Check class definitions are well-formed (using the trait definitions) *)

open Ast.Ast_types
open Core

val type_class_defns : class_defn list -> trait_defn list -> unit Or_error.t
