open Core
open Runtime_env

val compile_program : Typed_ast.program -> (code * stack * heap) Or_error.t
