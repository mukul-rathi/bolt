open Core

val ir_gen_program : Desugaring.Desugared_ast.program -> Llvm_ast.program Or_error.t
val pprint_llvm_ast : Format.formatter -> Llvm_ast.program -> unit
