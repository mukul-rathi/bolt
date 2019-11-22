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

type capability = Linear | Thread | Read

let string_of_cap = function Linear -> "Linear" | Thread -> "Thread" | Read -> "Read"

type cap_trait = TCapTrait of capability * Trait_name.t
type mode = MConst | MVar

let string_of_mode = function MConst -> "Const" | MVar -> "Var"

type type_field = TFieldInt

type type_expr =
  | TEInt
  | TEClass    of Class_name.t
  | TECapTrait of cap_trait
  | TEFun      of type_expr * type_expr

let rec string_of_type = function
  | TEInt                                    -> "Int"
  | TEClass class_name                       -> Fmt.str "Class: %s"
                                                  (Class_name.to_string class_name)
  | TECapTrait (TCapTrait (cap, trait_name)) ->
      Fmt.str "CapTrait: %s %s" (string_of_cap cap) (Trait_name.to_string trait_name)
  | TEFun (arg, body)                        -> Fmt.str "%s -> %s" (string_of_type arg)
                                                  (string_of_type body)

type field_defn = TField of mode * Field_name.t * type_field
type require_field_defn = TRequire of field_defn
type class_defn = TClass of Class_name.t * cap_trait * field_defn list
type trait_defn = TTrait of Trait_name.t * capability * require_field_defn list
