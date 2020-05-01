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

let string_of_maybe_borrowed_ref = function Some Borrowed -> "Borrowed " | None -> ""

(* If class is type-parameterised *)
type generic_type = Generic

let string_of_maybe_generic = function Some Generic -> "<T>" | None -> ""

let string_of_maybe_inherits = function
  | Some class_name -> Fmt.str " extends %s" (Class_name.to_string class_name)
  | None            -> ""

type type_expr =
  | TEInt
  | TEClass   of Class_name.t * type_expr option  (** optionally specify type parameters *)
  | TEVoid
  | TEBool
  | TEGeneric

let rec string_of_type = function
  | TEInt -> "Int"
  | TEClass (class_name, maybe_type_param) ->
      let maybe_type_param_str =
        match maybe_type_param with
        | Some type_param -> Fmt.str "<%s>" (string_of_type type_param)
        | None            -> "" in
      Fmt.str "%s%s" (Class_name.to_string class_name) maybe_type_param_str
  | TEVoid -> "Void"
  | TEBool -> "Bool"
  | TEGeneric -> "T"

type field_defn = TField of modifier * type_expr * Field_name.t * Capability_name.t list
type capability = TCapability of mode * Capability_name.t

let string_of_cap (TCapability (mode, cap_name)) =
  Fmt.str "%s %s" (string_of_mode mode) (Capability_name.to_string cap_name)

type param =
  | TParam of type_expr * Var_name.t * Capability_name.t list option * borrowed_ref option

let get_params_types params =
  List.map ~f:(fun (TParam (param_type, _, _, _)) -> param_type) params

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

(* Exceptions *)

exception NotDesugaredGenericType of string

(* Thrown if a later compiler stage encounters generic types when it expects it to be
   desugared *)
