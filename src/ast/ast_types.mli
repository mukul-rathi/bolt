(** These are the types used in constructing any AST*)

type loc = Lexing.position
(** Stores the line and position of the token *)

val string_of_loc : loc -> string

(** Distinguish between different identifiers *)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Var_name : ID
module Class_name : ID
module Trait_name : ID
module Field_name : ID

(** Define capabilities for data references *)
type capability = Linear | Thread | Read

val string_of_cap : capability -> string

(** Associate capabilities with threads *)
type cap_trait = TCapTrait of capability * Trait_name.t

(** Determines whether field is (im)mutable *)
type mode = MConst | MVar

val string_of_mode : mode -> string

type type_field = TFieldInt

type type_expr =
  | TEInt
  | TEClass    of Class_name.t
  | TECapTrait of cap_trait
  | TEFun      of type_expr * type_expr

val string_of_type : type_expr -> string

type field_defn = TField of mode * Field_name.t * type_field
type require_field_defn = TRequire of field_defn
type class_defn = TClass of Class_name.t * cap_trait * field_defn list
type trait_defn = TTrait of Trait_name.t * capability * require_field_defn list
