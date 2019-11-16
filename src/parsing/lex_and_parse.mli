open Core
(** This module executes the lexer and parser. It acts as an interface between the
    parsing code and the main method (abstracting away the underlying implementation) *)

val parse_program : string -> (Ast_types.program, Error.t) Result.t
(** Given the path to a bolt program, parse the program and return the AST if successful*)

val pprint_ast : Format.formatter -> Ast_types.program -> unit
(** Given a formatter and AST, pretty-print the AST - useful for debugging *)
