(** Check class definitions are well-formed *)

open Ast_types
open Core

val type_class_defns : class_defn list -> trait_defn list -> (unit, Error.t) result
