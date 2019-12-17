(** These are the types used in constructing any AST for a Bolt program. Bolt programs
    consist of a list of class definitions, followed by a list of trait definitions and
    finally an expression to execute. *)

type loc = Lexing.position
(** Stores the line and position of the token *)

(** An abstract type for identifiers*)
module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

(** We distinguish the different identifiers for variables, classes, traits and fields. *)

module Var_name : ID
module Class_name : ID
module Trait_name : ID
module Field_name : ID
module Function_name : ID

(** Define capabilities for data references *)
type capability =
  | Linear  (** Only permitted one alias to the object at any time *)
  | Thread  (** Permitted multiple aliases but only within the same thread *)
  | Read  (** Allowed access through multiple aliases but the object must be immutable *)

(** Associate capabilities with traits *)
type cap_trait = TCapTrait of capability * Trait_name.t

(** Determines whether field is (im)mutable *)
type mode = MConst  (** Immutable *) | MVar  (** Mutable *)

(** Types of fields defined in classes/traits *)
type type_field = TFieldInt

(** Define types of expressions in Bolt programs*)
type type_expr = TEInt | TEClass of Class_name.t | TECapTrait of cap_trait

(** Class Field declarations are of the form "mode name : type" e.g. const f : int *)
type field_defn = TField of mode * Field_name.t * type_field

(** Trait Field declarations are similar, except they're of the form "require mode : type"
    e.g. require const f : int *)
type require_field_defn = TRequire of field_defn

(** Trait definitions consist of name and the capability and a list of the fields it
    requires *)
type trait_defn = TTrait of Trait_name.t * capability * require_field_defn list

(** Various helper functions to convert types to equivalent string representations *)

(** Parameter of a function *)
type param = TParam of type_expr * Var_name.t

val string_of_loc : loc -> string
val string_of_cap : capability -> string
val string_of_mode : mode -> string
val string_of_type : type_expr -> string
