open Core
open Parsing

val type_function_defns :
     Parsed_ast.class_defn list
  -> Parsed_ast.function_defn list
  -> Typed_ast.function_defn list Or_error.t
(** Type-check function defns, using types defined in the class defns, and return the
    typed function defn augmented with type information *)
