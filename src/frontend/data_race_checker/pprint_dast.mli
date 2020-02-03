(** This module pretty prints the desugared AST of a Bolt program *)

val pprint_program : Format.formatter -> Data_race_checker_ast.program -> unit
