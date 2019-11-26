(** This module takes in a typed AST and then performs a second stage of type-checking,
    only this time it is type-checking references for data-race freedom *)

open Core

val type_data_race : Typed_ast.program -> unit Or_error.t
