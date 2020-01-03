(** This module pretty prints the parsed AST of a Bolt program *)

val pprint_program : Format.formatter -> Parsed_ast.program -> unit
