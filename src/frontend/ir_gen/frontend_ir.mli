(** This IR is serialised to Protobuf, to be converted to LLVM IR in the middle/backend of
    the compiler. Therefore we simplify the types used to make it easier to deserialise.

    We drop:

    - type information about the expressions (only keeping function / method types)
    - the position (loc) since these were used for type error debugging.
    - modes / capabilities (as these are only used in the data-race type checker)
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

type lock_type = Reader [@key 1] | Writer [@key 2] [@key 3] [@@deriving protobuf]

val string_of_lock_type : lock_type -> string

type expr =
  | Integer     of int [@key 1]
  | Boolean     of bool [@key 2]
  | Identifier  of identifier * lock_type option [@key 3]
  | Constructor of string * constructor_arg list [@key 4]
  | Let         of string * expr [@key 5]
  | Assign      of identifier * expr * lock_type option [@key 6]
  | Consume     of identifier * lock_type option [@key 7]
  | FunctionApp of string * expr list [@key 8]
  | MethodApp   of string * string * int * expr list [@key 18]
      (** object name, static method name (used to get method arg types), method index
          into vtable, args *)
  | Printf      of string * expr list [@key 9]
  | FinishAsync of async_expr list * expr list [@key 10]
  | IfElse      of expr * expr list * expr list [@key 11]  (** If ___ then ___ else ___ *)
  | WhileLoop   of expr * expr list [@key 12]  (** While ___ do ___ ; *)
  | BinOp       of bin_op * expr * expr [@key 13]
  | UnOp        of un_op * expr [@key 14]
  | Block       of block_expr [@key 15]
  | Lock        of string * lock_type [@key 16]
  | Unlock      of string * lock_type [@key 17]
[@@deriving protobuf]

and block_expr = expr list [@@deriving protobuf]

(** Async exprs have a precomputed list of their free variables (passed as arguments when
    they are spawned as thread) *)
and async_expr = AsyncExpr of string list * block_expr [@key 1] [@@deriving protobuf]

(** [int]=field_index, [expr] = assigned value *)
and constructor_arg = ConstructorArg of int * expr [@key 1] [@@deriving protobuf]

(** Function defn consists of the function name, return type, the list of params, and the
    body expr block of the function *)
type function_defn = TFunction of string * type_expr * param list * expr list [@key 1]
[@@deriving protobuf]

(** Class definitions consist of the class name, the list of the types of its fields and a
    vtable. Its methods are now plain old functions *)
type class_defn = TClass of string * type_expr list * string list [@key 1]
[@@deriving protobuf]

(** Each bolt program defines the classes,followed by functions, followed by the main
    expression block to execute. *)
type program = Prog of class_defn list * function_defn list * expr list [@key 1]
[@@deriving protobuf]
