open Core
open Ast.Ast_types

val type_function_defns :
     Parsing.Parsed_ast.class_defn list
  -> trait_defn list
  -> Parsing.Parsed_ast.function_defn list
  -> Typed_ast.function_defn list Or_error.t
