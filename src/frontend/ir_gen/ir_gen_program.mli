(** Given the desugared AST of a bolt program, convert it to serialisable IR (to be passed
    to the compiler middle/backend). *)

open Core

val ir_gen_program : Desugaring.Desugared_ast.program -> Llvm_ast.program Or_error.t

val pprint_llvm_ast : Format.formatter -> Llvm_ast.program -> unit
(** Given a formatter and the generated IR, pretty-print the IR - useful for debugging *)
