(** This AST is serialised to Protobuf, to be converted to LLVM IR in the middle/backend
    of the compiler. Therefore we simplify the types used to make it easier to
    deserialise.

    We drop:

    - type information about the expressions (only keeping function / method types)
    - the position (loc) since these were used for type error debugging.
    - capabilities / region effects (as these are only used in the data-race type checker)
    - Const / Var modifiers for fields (again, these are used in Type Checking)

    We also use strings for identifiers rather than the abstract ID signatures,

    Methods are converted to functions, with the first argument being "this"

    We copy across un_op / bin_op type information, as although these are the only AST
    types that remain unchanged, it makes sense to keep the AST interface in one file. *)

type un_op = UnOpNot | UnOpNeg

val string_of_un_op : un_op -> string

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

val string_of_bin_op : bin_op -> string

type type_expr = TEInt | TEClass of string | TEVoid | TEBool

val string_of_type : type_expr -> string

type param = TParam of type_expr * string | TVoid
type field_defn = TField of type_expr * string

type identifier =
  | Variable of string
  | ObjField of string * string  (** object name, field *)

type expr =
  | Unit
  | Integer     of int  (** no need for type_expr annotation as obviously TEInt *)
  | Boolean     of bool  (** no need for type_expr annotation as obviously TEBool *)
  | Identifier  of identifier  (** Type information associated with identifier *)
  | Constructor of string * constructor_arg list
  | Let         of string * expr
  | Assign      of identifier * expr
  | Consume     of identifier  (** Type is associated with the identifier *)
  | FunctionApp of string * expr list
  | FinishAsync of expr list list * expr list
      (** overall type is that of the expr on the current thread - since forked exprs'
          values are ignored *)
  | If          of expr * expr list * expr list
      (** If ___ then ___ else ___ - type is that of the branch exprs *)
  | While       of expr * expr list
      (** While ___ do ___ ; - no need for type_expr annotation as type of a loop is
          TEVoid *)
  | BinOp       of bin_op * expr * expr
  | UnOp        of un_op * expr

and constructor_arg = ConstructorArg of string * expr

(** Function defn consists of the function name, return type, the list of params, and the
    body expr block of the function *)
type function_defn = TFunction of string * type_expr * param list * expr list

(** Class definitions consist of the class name and its fields. Its methods are now plain
    old functions *)
type class_defn = TClass of string * field_defn list

(** Each bolt program defines the classes,followed by functions, followed by the main
    expression block to execute. *)
type program = Prog of class_defn list * function_defn list * expr list
