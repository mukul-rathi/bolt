(** Given the parsed AST of a bolt program, type-check it and if type-checking succeeds
    return the AST with type annotations.

    NB: We are not checking for data-race freedom in this stage of the type-checker. *)

open Core

val type_program : Parsing.Parsed_ast.program -> Typed_ast.program Or_error.t

val pprint_typed_ast : Format.formatter -> Typed_ast.program -> unit
(** Given a formatter and typed AST, pretty-print the AST - useful for debugging *)
