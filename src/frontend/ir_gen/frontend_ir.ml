(* This AST is serialised to Protobuf, to be converted to LLVM IR in the middle/backend of
   the compiler. Therefore we simplify the types used to make it easier to deserialise.

   We drop:

   - type information about the expressions (only keeping function / method types) - the
   position (loc) since these were used for type error debugging. - capabilities / region
   effects (as these are only used in the data-race type checker) - Const / Var modifiers
   for fields (again, these are used in Type Checking) - field names - fields are now just
   indices into a class struct.

   We also use strings for identifiers rather than the abstract ID signatures,

   Methods are converted to functions, with the first argument being "this"

   We copy across un_op / bin_op type information, as although these are the only AST
   types that remain unchanged, it makes sense to keep the AST interface in one file. *)

type un_op = UnOpNot [@key 1] | UnOpNeg [@key 2]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

let string_of_un_op = function UnOpNot -> "!" | UnOpNeg -> "-"

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
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

let string_of_bin_op = function
  | BinOpPlus          -> "+"
  | BinOpMinus         -> "-"
  | BinOpMult          -> "*"
  | BinOpIntDiv        -> "/"
  | BinOpRem           -> "%"
  | BinOpLessThan      -> "<"
  | BinOpLessThanEq    -> "<="
  | BinOpGreaterThan   -> ">"
  | BinOpGreaterThanEq -> ">="
  | BinOpAnd           -> "&&"
  | BinOpOr            -> "||"
  | BinOpEq            -> "=="
  | BinOpNotEq         -> "!="

type type_expr =
  | TEInt [@key 1]
  | TEClass of string [@key 2]
  | TEVoid [@key 3]
  | TEBool [@key 4]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

let string_of_type = function
  | TEInt              -> "Int"
  | TEClass class_name -> Fmt.str "Class: %s" class_name
  | TEVoid             -> "Void"
  | TEBool             -> "Bool"

type param = TParam of type_expr * string [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

type identifier =
  | Variable of string [@key 1]
  | ObjField of string * int [@key 2] (* object name, field *)
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

type expr =
  | Integer     of int [@key 1]
  | Boolean     of bool [@key 2]
  | Identifier  of identifier * bool (* whether to lock or not *) [@key 3]
  | Constructor of string * constructor_arg list [@key 4]
  | Let         of string * expr [@key 5]
  | Assign      of identifier * expr * bool (* whether to lock or not *) [@key 6]
  | Consume     of identifier * bool (* whether to lock or not *) [@key 7]
  | FunctionApp of string * exprs [@key 8]
  | MethodApp   of string * string * expr list [@key 18]
  (* object name, method name, args *)
  | Printf      of string * exprs [@key 9]
  | FinishAsync of async_expr list * exprs [@key 10]
  | IfElse      of expr * exprs * exprs [@key 11]
  (* If ___ then ___ else ___ *)
  | WhileLoop   of expr * exprs [@key 12]
  (* While ___ do ___ ; *)
  | BinOp       of bin_op * expr * expr [@key 13]
  | UnOp        of un_op * expr [@key 14]
  | Block       of exprs [@key 15]
  | Lock        of string [@key 16]
  | Unlock      of string [@key 17]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

and exprs = expr list (* Helper type to generate protobuf for exprs list *)
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

(* Async exprs have a precomputed list of their free variables (passed as arguments when
   they are spawned as thread) *)
and async_expr = AsyncExpr of string list * exprs [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

and constructor_arg = ConstructorArg of int * expr [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

(* Function defn consists of the function name, return type, the list of params, and the
   body expr block of the function *)
type function_defn = TFunction of string * type_expr * param list * expr list [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

(* Class definitions consist of the class name and list of the types of its fields. Its
   methods are now plain old functions *)
type class_defn = TClass of string * type_expr list [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]

(* Each bolt program defines the classes,followed by functions, followed by the main
   expression block to execute. *)
type program = Prog of class_defn list * function_defn list * expr list [@key 1]
[@@deriving protobuf {protoc= "../../frontend_ir.proto"}]
