(** Given the parsed AST of a bolt program, type-check it and if type-checking succeeds
    return the AST with type annotations.

    NB: We are not checking for data-race freedom in this stage of the type-checker. *)

open Core

val type_core_lang : Parsed_ast.program -> Typed_ast.program Or_error.t
