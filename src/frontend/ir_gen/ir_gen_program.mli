(** Given the desugared AST of a bolt program, convert it to serialisable IR (to be passed
    to the compiler middle/backend). *)

open Core

val ir_gen_program :
  Data_race_checker.Data_race_checker_ast.program -> Frontend_ir.program Or_error.t

val pprint_frontend_ir : Format.formatter -> Frontend_ir.program -> unit
(** Given a formatter and the generated IR, pretty-print the IR - useful for debugging *)
