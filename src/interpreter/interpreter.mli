(** The interpreter takes as input the typed AST of the program and first compiles it to
    an intermediate instruction representation and an initial state.

    It then executes these instructions and optionally prints the state at each step of
    the execution, before finally returning the final value if the computation succeeds,
    or a runtime error *)

open Core
open Typing_core_lang

val run_program :
     Typed_ast.program
  -> print_execution:Format.formatter option
  -> Runtime_env.value Or_error.t

val print_result : Format.formatter -> Runtime_env.value -> unit
