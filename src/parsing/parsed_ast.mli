(** These are the types used in constructing the AST*)

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

type loc = Lexing.position
(** Stores the line and position of the token *)

(** Define capabilities for data references *)
type capability = Linear | Thread | Read

(** Associate capabilities with threads *)
type cap_trait = TCapTrait of capability * Trait_name.t

(** Determines whether field is (im)mutable *)
type mode = MConst | MVar

type type_field = TFieldInt

(** Fields are defined in a class *)
type field_defn = TField of mode * Field_name.t * type_field

(** Corresponding field required by any class implementing that trait *)
type require_field_defn = TRequire of field_defn

type type_expr =
  | TEInt
  | TEClass    of Class_name.t
  | TECapTrait of cap_trait
  | TEFun      of type_expr * type_expr

type class_defn = TClass of Class_name.t * cap_trait * field_defn list
type trait_defn = TTrait of Trait_name.t * capability * require_field_defn list

(** Possible types of executable expressions *)
type expr =
  | Integer     of loc * int
  | Variable    of loc * Var_name.t
  | Lambda      of loc * Var_name.t * type_expr * expr
  | App         of loc * expr * expr
  | Seq         of loc * expr list
  | Let         of loc * Var_name.t * expr * expr
  | ObjField    of loc * Var_name.t * Field_name.t
  | Assign      of loc * Var_name.t * Field_name.t * expr
  | Constructor of loc * Class_name.t * constructor_args list
  | Consume     of loc * Var_name.t
  | FinishAsync of loc * expr * expr * expr

and constructor_args = ConstructorArgs of Field_name.t * expr

(** Each bolt program defines the classes, followed by the traits, followed by the
    expression to execute. *)
type program = Prog of class_defn list * trait_defn list * expr
