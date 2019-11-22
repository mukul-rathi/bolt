open Core
(** This module executes the lexer and parser. It acts as an interface between the
    parsing code and the main method (abstracting away the underlying implementation) *)

val parse_program : string -> (Parsed_ast.program, Error.t) Result.t
(** Given the path to a bolt program, parse the program and return the AST if successful*)

val pprint_parsed_ast : Format.formatter -> Parsed_ast.program -> unit
(** Given a formatter and parsed AST, pretty-print the AST - useful for debugging *)
