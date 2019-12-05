(** This module executes the lexer and parser. It acts as an interface between the
    parsing code and the main method (abstracting away the underlying implementation) *)

open Core

val parse_program : In_channel.t -> Parsed_ast.program Or_error.t
(** Given a channel to read a bolt program from, parse the program and return the AST if
    successful*)

val pprint_parsed_ast : Format.formatter -> Parsed_ast.program -> unit
(** Given a formatter and parsed AST, pretty-print the AST - useful for debugging *)
