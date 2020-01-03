(** This module is used by all ir_gen expect tests *)

val print_llvm_ast : string -> unit
(** Given a bolt program as a string, print out its last frontend AST - to be converted to
    LLVM IR *)
