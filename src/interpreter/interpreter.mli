open Core

val run_program :
     Typed_ast.program
  -> print_execution:Format.formatter option
  -> (Runtime_env.value, Error.t) result

val print_result : Format.formatter -> Runtime_env.value -> unit
