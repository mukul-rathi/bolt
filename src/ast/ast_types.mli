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
module Region_name : ID
module Field_name : ID
module Method_name : ID
module Function_name : ID

(** Define capabilities for data references *)
type capability =
  | Linear  (** Only permitted one alias to the object at any time *)
  | Thread  (** Permitted multiple aliases but only within the same thread *)
  | Read  (** Allowed access through multiple aliases but the object must be immutable *)
  | Subordinate
      (** Only accessible from within encapsulating object - inherits capability of owner *)
  | Locked
      (** Freely sharable amongst threads, operations protected by a lock on the object *)

(** Determines whether field is (im)mutable *)
type mode = MConst  (** Immutable *) | MVar  (** Mutable *)

(** Define types of expressions in Bolt programs*)
type type_expr = TEInt | TEClass of Class_name.t | TEVoid | TEBool

(** Class Field declarations are of the form "mode type name : regions" e.g. const int f :
    reg_1 *)
type field_defn = TField of mode * type_expr * Field_name.t * Region_name.t list

(** Regions consist of name and the capability *)
type region = TRegion of capability * Region_name.t

(** Parameter of a function optionally has a region guard *)
type param = TParam of type_expr * Var_name.t * Region_name.t list option | TVoid

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
val string_of_cap : capability -> string
val string_of_mode : mode -> string
val string_of_type : type_expr -> string
val string_of_bin_op : bin_op -> string
val string_of_un_op : un_op -> string
