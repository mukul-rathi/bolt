(** This module pretty prints the serialisable IR AST of a Bolt program *)

val pprint_program : Format.formatter -> Llvm_ast.program -> unit
