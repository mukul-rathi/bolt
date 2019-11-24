open Core

val type_core_lang : Parsed_ast.program -> (Typed_ast.program, Error.t) Result.t
