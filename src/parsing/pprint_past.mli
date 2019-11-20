open Parsed_ast

val pprint_program : Format.formatter -> program -> unit

val pprint_class_defn : Format.formatter -> string -> class_defn -> unit
(** All subsequent pretty print expressions will occur at an indent passed in as a string*)

val pprint_trait_defn : Format.formatter -> string -> trait_defn -> unit
val pprint_capability : Format.formatter -> string -> capability -> unit
val pprint_cap_trait : Format.formatter -> string -> cap_trait -> unit
val pprint_mode : Format.formatter -> string -> mode -> unit
val pprint_type_field : Format.formatter -> string -> type_field -> unit
val pprint_field_defn : Format.formatter -> string -> field_defn -> unit
val pprint_require_field_defn : Format.formatter -> string -> require_field_defn -> unit
val pprint_type_expr : Format.formatter -> string -> type_expr -> unit
val pprint_expr : Format.formatter -> string -> expr -> unit
