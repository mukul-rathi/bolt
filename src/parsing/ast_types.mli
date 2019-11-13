(** These are the types used in constructing the AST*)

type var_name = string
(** Distinguish between different identifiers *)

type class_name = string
type trait_name = string
type field_name = string

type loc = Lexing.position
(** Stores the line and position of the token *)

(** Define capabilities for data references *)
type capability = Linear | Thread | Read

(** Associate capabilities with threads *)
type cap_trait = TCapTrait of capability * trait_name

(** Determines whether field is (im)mutable *)
type mode = MConst | MVar

type type_field = TFieldInt

(** Fields are defined in a class *)
type field_defn = TField of mode * field_name * type_field

(** Corresponding field required by any class implementing that trait *)
type require_field_defn = TRequire of field_defn

type type_expr =
  | TEInt
  | TEClass    of class_name
  | TECapTrait of cap_trait
  | TEFun      of type_expr * type_expr

type class_defn = TClass of class_name * cap_trait * field_defn list
type trait_defn = TTrait of trait_name * capability * require_field_defn list

(** Possible types of executable expressions *)
type expr =
  | Null        of loc
  | Integer     of loc * int
  | Variable    of loc * var_name
  | Lambda      of loc * var_name * type_expr * expr
  | App         of loc * expr * expr
  | Seq         of loc * expr list
  | Let         of loc * var_name * type_expr * expr * expr
  | ObjField    of loc * var_name * field_name
  | Assign      of loc * var_name * field_name * expr
  | Constructor of loc * class_name * constructor_args list
  | Consume     of loc * var_name
  | FinishAsync of loc * expr * expr * expr

and constructor_args = ConstructorArgs of field_name * expr

(** Each bolt program defines the classes, followed by the traits, followed by the
    expression to execute. *)
type program = Prog of class_defn list * trait_defn list * expr
