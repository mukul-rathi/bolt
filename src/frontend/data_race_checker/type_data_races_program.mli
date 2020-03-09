(** This module is responsible for checking the the desugared AST for data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_program : ignore_data_races:bool -> program -> program Or_error.t
(** If ignore_data_races flag set, will check capabilities but won't enforce constraints. *)

val pprint_data_race_checker_ast : Format.formatter -> program -> unit
(** Given a formatter and desugared AST, pretty-print the AST - useful for debugging *)
