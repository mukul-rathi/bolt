open Base

type loc = Lexing.position

let string_of_loc loc =
  Fmt.str "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.( = )
end

module Var_name : ID = String_id
module Class_name : ID = String_id
module Capability_name : ID = String_id
module Field_name : ID = String_id
module Method_name : ID = String_id
module Function_name : ID = String_id

type mode =
  | Linear
  | ThreadLocal
  | Read
  | Locked
  | ThreadSafe
  | Subordinate
  | Encapsulated

let string_of_mode = function
  | Linear       -> "Linear"
  | ThreadLocal  -> "ThreadLocal"
  | Read         -> "Read"
  | Locked       -> "Locked"
  | ThreadSafe   -> "ThreadSafe"
  | Subordinate  -> "Subordinate"
  | Encapsulated -> "Encapsulated"

type modifier = MConst | MVar

let string_of_modifier = function MConst -> "Const" | MVar -> "Var"

(* determines if a reference is being borrowed for that scope *)
type borrowed_ref = Borrowed
type type_expr = TEInt | TEClass of Class_name.t | TEVoid | TEBool

let string_of_type = function
  | TEInt              -> "Int"
  | TEClass class_name -> Fmt.str "Class: %s" (Class_name.to_string class_name)
  | TEVoid             -> "Void"
  | TEBool             -> "Bool"

type field_defn = TField of modifier * type_expr * Field_name.t * Capability_name.t list
type capability = TCapability of mode * Capability_name.t

type param =
  | TParam of type_expr * Var_name.t * Capability_name.t list option * borrowed_ref option

(* BINARY OPERATORS *)

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

type un_op = UnOpNot | UnOpNeg

let string_of_un_op = function UnOpNot -> "!" | UnOpNeg -> "-"
