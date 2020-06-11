(** These are the types used in constructing any AST for a Bolt program. Bolt programs
    consist of a list of class definitions, followed by a list of function definitions and
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
module Capability_name : ID
module Field_name : ID
module Method_name : ID
module Function_name : ID

(** Define modes for data references *)
type mode =
  | Linear  (** Only permitted one alias to the object at any time *)
  | ThreadLocal  (** Permitted multiple aliases but only within the same thread *)
  | Read  (** Allowed access through multiple aliases but the object must be immutable *)
  | Locked
      (** Freely sharable amongst threads, operations protected by a lock on the object *)
  | ThreadSafe
      (** Abstracts locked + read - these are objects that are allowed access in multiple
          threads *)
  | Subordinate
      (** Only accessible from within encapsulating object - inherits capability of owner *)
  | Encapsulated  (** All capabilities are subordinate - fully encapsulated *)

(** Determines whether field is (im)mutable *)
type modifier = MConst  (** Immutable *) | MVar  (** Mutable *)

(** determines if a reference is being borrowed for that scope *)
type borrowed_ref = Borrowed

(** If class is type-parameterised *)
type generic_type = Generic

(** Define types of expressions in Bolt programs*)
type type_expr =
  | TEInt
  | TEClass   of Class_name.t * type_expr option
      (** optionally specify type parameters - for generic classes *)
  | TEVoid
  | TEBool
  | TEGeneric

(** Class Field declarations are of the form "modifier type name : capabilities" e.g.
    const int f : cap_1 *)
type field_defn = TField of modifier * type_expr * Field_name.t * Capability_name.t list

(** Capabilities consist of a mode and a name *)
type capability = TCapability of mode * Capability_name.t

(** Parameter of a function can optionally restrict capabilities used, and may be
    "borrowed". *)
type param =
  | TParam of type_expr * Var_name.t * Capability_name.t list option * borrowed_ref option

val get_params_types : param list -> type_expr list

(** Binary operators for expressions *)

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpIntDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpLessThanEq
  | BinOpGreaterThan
  | BinOpGreaterThanEq
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNotEq

type un_op = UnOpNot | UnOpNeg

(** Various helper functions to convert types to equivalent string representations *)

val string_of_loc : loc -> string
val string_of_mode : mode -> string
val string_of_cap : capability -> string
val string_of_modifier : modifier -> string
val string_of_type : type_expr -> string
val string_of_bin_op : bin_op -> string
val string_of_un_op : un_op -> string
val string_of_maybe_borrowed_ref : borrowed_ref option -> string
val string_of_maybe_generic : generic_type option -> string
val string_of_maybe_superclass : Class_name.t option -> string

(** Exceptions *)

exception NotDesugaredGenericType of string
(** Thrown if a later compiler stage encounters generic types when it expects it to be
    desugared *)
