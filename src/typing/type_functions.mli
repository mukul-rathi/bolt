open Core

val type_function_defns :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.function_defn list
  -> Typed_ast.function_defn list Or_error.t
(** Type-check function defns, using types defined in the class defns, and return the
    typed function defn augmented with type information *)
