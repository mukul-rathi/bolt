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
module Trait_name : ID = String_id
module Field_name : ID = String_id
module Function_name : ID = String_id

type capability = Linear | Thread | Read

let string_of_cap = function Linear -> "Linear" | Thread -> "Thread" | Read -> "Read"

type cap_trait = TCapTrait of capability * Trait_name.t
type mode = MConst | MVar

let string_of_mode = function MConst -> "Const" | MVar -> "Var"

type type_field = TFieldInt | TFieldBool

type type_expr =
  | TEInt
  | TEClass    of Class_name.t
  | TECapTrait of cap_trait
  | TEUnit
  | TEBool

let string_of_type = function
  | TEInt -> "Int"
  | TEClass class_name -> Fmt.str "Class: %s" (Class_name.to_string class_name)
  | TECapTrait (TCapTrait (cap, trait_name)) ->
      Fmt.str "CapTrait: %s %s" (string_of_cap cap) (Trait_name.to_string trait_name)
  | TEUnit -> "()"
  | TEBool -> "Bool"

type field_defn = TField of mode * Field_name.t * type_field
type require_field_defn = TRequire of field_defn
type trait_defn = TTrait of Trait_name.t * capability * require_field_defn list
type param = TParam of type_expr * Var_name.t | TVoid

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

type un_op = UnOpNot

let string_of_un_op UnOpNot = "!"
