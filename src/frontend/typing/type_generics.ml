open Core
open Ast.Ast_types

let instantiate_maybe_generic_this
    (Parsing.Parsed_ast.TClass (class_name, maybe_generic, _, _, _)) =
  let maybe_type_param =
    match maybe_generic with Some Generic -> Some TEGeneric | None -> None in
  (Var_name.of_string "this", TEClass (class_name, maybe_type_param))

let instantiate_maybe_generic_class_defn
    ( Parsing.Parsed_ast.TClass
        (class_name, maybe_generic, caps, field_defns, method_defns) as class_defn )
    maybe_type_param loc =
  match (maybe_generic, maybe_type_param) with
  | None, None -> Ok class_defn
  | None, Some type_param ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - non-generic class %s is being instantiated with a type parameter %s@."
              (string_of_loc loc)
              (Class_name.to_string class_name)
              (string_of_type type_param)))
  | Some Generic, None ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - generic class %s needs to be instantiated with a type parameter@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))
  | Some Generic, Some _type_param ->
      Ok
        (Parsing.Parsed_ast.TClass
           (class_name, maybe_generic, caps, field_defns, method_defns))
