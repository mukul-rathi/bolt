(** This module pretty prints the last IR AST of a Bolt program *)

val pprint_program : Format.formatter -> Llvm_ast.program -> unit
