(** This module takes the typed AST of a bolt program and converts it to a list of
    instructions to be executed by the interpreter, as well as setting the initial stack
    and heap of the interpreter. *)

open Core
open Runtime_env

val compile_program : Typed_ast.program -> (code * stack * heap) Or_error.t
