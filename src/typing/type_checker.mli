(** The type checker takes a Parsed AST and type-checks the core language rules,
    returning a typed AST if type-checking succeeds. It also optionally performs a second
    stage of type-checking to check for data-race freedom. *)

open Core

val type_check_program :
     Parsing.Parsed_ast.program
  -> check_data_races:bool
  -> Typing_core_lang.Typed_ast.program Or_error.t

val pprint_typed_ast : Format.formatter -> Typing_core_lang.Typed_ast.program -> unit
(** Given a formatter and typed AST, pretty-print the AST - useful for debugging *)
