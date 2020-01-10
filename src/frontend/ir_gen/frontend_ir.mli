(** This IR is serialised to Protobuf, to be converted to LLVM IR in the middle/backend of
    the compiler. Therefore we simplify the types used to make it easier to deserialise.

    We drop:

    - type information about the expressions (only keeping function / method types)
    - the position (loc) since these were used for type error debugging.
    - capabilities / region effects (as these are only used in the data-race type checker)
    - Const / Var modifiers for fields (again, these are used in Type Checking)
    - field names - fields are now just indices into a class struct.

    We also use strings for identifiers rather than the abstract ID signatures,

    Methods are converted to functions, with the first argument being "this"

    We copy across un_op / bin_op type information, as although these are the only AST
    types that remain unchanged, it makes sense to keep the AST interface in one file. *)

type un_op = UnOpNot [@key 1] | UnOpNeg [@key 2] [@@deriving protobuf]

val string_of_un_op : un_op -> string

type bin_op =
  | BinOpPlus [@key 1]
  | BinOpMinus [@key 2]
  | BinOpMult [@key 3]
  | BinOpIntDiv [@key 4]
  | BinOpRem [@key 5]
  | BinOpLessThan [@key 6]
  | BinOpLessThanEq [@key 7]
  | BinOpGreaterThan [@key 8]
  | BinOpGreaterThanEq [@key 9]
  | BinOpAnd [@key 10]
  | BinOpOr [@key 11]
  | BinOpEq [@key 12]
  | BinOpNotEq [@key 13]
[@@deriving protobuf]

val string_of_bin_op : bin_op -> string

type type_expr =
  | TEInt [@key 1]
  | TEClass of string [@key 2]
  | TEVoid [@key 3]
  | TEBool [@key 4]
[@@deriving protobuf]

val string_of_type : type_expr -> string

type param = TParam of type_expr * string [@key 1] [@@deriving protobuf]

type identifier =
  | Variable of string [@key 1]
  | ObjField of string * int [@key 2]
      (** object name, field = index into class field types list *)
[@@deriving protobuf]

type expr =
  | Integer     of int [@key 2]
  | Boolean     of bool [@key 3]
  | Identifier  of identifier [@key 4]
  | Constructor of string * constructor_arg list [@key 5]  (** [string]=class_name *)
  | Let         of string * expr [@key 6]  (** [string]=var_name *)
  | Assign      of identifier * expr [@key 7]
  | Consume     of identifier [@key 8]
  | FunctionApp of string * exprs [@key 9]  (** [string]=function_name *)
  | FinishAsync of exprs list * exprs [@key 10]
  | IfElse      of expr * exprs * exprs [@key 11]
      (** If ___ then ___ else ___ note - renamed to avoid keyword clash in generated
          protobuf c++ *)
  | WhileLoop   of expr * exprs [@key 12]
      (** While ___ do ___ ; note - renamed to avoid keyword clash in generated protobuf
          c++ *)
  | BinOp       of bin_op * expr * expr [@key 13]
  | UnOp        of un_op * expr [@key 14]
[@@deriving protobuf]

and exprs = expr list [@@deriving protobuf]
(** Helper type to generate protobuf for expr list list *)

(** [int]=field_index, [expr] = assigned value *)
and constructor_arg = ConstructorArg of int * expr [@key 1] [@@deriving protobuf]

(** Function defn consists of the function name, return type, the list of params, and the
    body expr block of the function *)
type function_defn = TFunction of string * type_expr * param list * expr list [@key 1]
[@@deriving protobuf]

(** Class definitions consist of the class name and a list of the types of its fields.
    Methods are now plain old functions and not associated with classes *)
type class_defn = TClass of string * type_expr list [@key 1] [@@deriving protobuf]

(** Each bolt program defines the classes,followed by functions, followed by the main
    expression block to execute. *)
type program = Prog of class_defn list * function_defn list * expr list [@key 1]
[@@deriving protobuf]
