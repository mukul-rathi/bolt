open Core

val type_check_program :
  Parsed_ast.program -> check_data_races:bool -> (Typed_ast.program, Error.t) Result.t

val pprint_typed_ast : Format.formatter -> Typed_ast.program -> unit
(** Given a formatter and typed AST, pretty-print the AST - useful for debugging *)
