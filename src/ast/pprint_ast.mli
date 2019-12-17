(** This module defines a set of pretty printing functions for the parts of the AST that
    are common across the parsed and typed ASTs - to be used by pretty printing functions
    of the two ASTs.

    They all take in a formatter which specifies output channel e.g. a string formatter or
    a stdout formatter. (Think of this as a generalisation of printf)

    The second argument for all of these is an indent - this corresponds to nesting depth
    within the AST *)

open Ast_types

val pprint_trait_defn : Format.formatter -> indent:string -> trait_defn -> unit
val pprint_capability : Format.formatter -> indent:string -> capability -> unit
val pprint_cap_trait : Format.formatter -> indent:string -> cap_trait -> unit
val pprint_mode : Format.formatter -> indent:string -> mode -> unit
val pprint_type_field : Format.formatter -> indent:string -> type_field -> unit
val pprint_field_defn : Format.formatter -> indent:string -> field_defn -> unit

val pprint_require_field_defn :
  Format.formatter -> indent:string -> require_field_defn -> unit

val pprint_type_expr : Format.formatter -> indent:string -> type_expr -> unit
val pprint_param : Format.formatter -> indent:string -> param -> unit
