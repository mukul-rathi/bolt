(** Given the desugared AST of a bolt program, convert it to serialisable IR (to be passed
    to the compiler middle/backend). *)

open Core
open Desugaring

val ir_gen_program : Desugared_ast.program -> Frontend_ir.program Or_error.t

val pprint_frontend_ir : Format.formatter -> Frontend_ir.program -> unit
(** Given a formatter and the generated IR, pretty-print the IR - useful for debugging *)
