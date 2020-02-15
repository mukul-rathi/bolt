(** This module is responsible for checking the the desugared AST for data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_program : program -> program Or_error.t

val pprint_data_race_checker_ast : Format.formatter -> program -> unit
(** Given a formatter and desugared AST, pretty-print the AST - useful for debugging *)
