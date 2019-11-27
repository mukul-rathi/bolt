(** The typed AST consists of the original AST but augmented with type information. *)

open Ast_types

(** Similar to Parsed AST, only we add an extra type_expr annotation to denote the
    overall type of the expression. *)
type expr =
  | Integer     of loc * int  (** no need for type_expr annotation as obviously TEInt *)
  | Variable    of loc * type_expr * Var_name.t
  | Lambda      of loc * type_expr * Var_name.t * type_expr * expr
  | App         of loc * type_expr * expr * expr
  | Seq         of loc * type_expr * expr list  (** type is of the final expr in seq *)
  | Let         of loc * type_expr * Var_name.t * expr * expr
      (** overall type is that of the body expr *)
  | ObjField    of loc * type_expr * Var_name.t * type_expr * Field_name.t
      (** First type is of the overall expr x.f, second is the type of the obj x *)
  | Assign      of loc * type_expr * Var_name.t * type_expr * Field_name.t * expr
      (** First type is of the expr, second is the type of the obj *)
  | Constructor of loc * type_expr * Class_name.t * constructor_arg list
  | Consume     of loc * type_expr * expr  (** type is that of the expr being consumed *)
  | FinishAsync of loc * type_expr * expr * expr * expr
      (** overall type is that of the next_expr *)

and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

type program = Prog of class_defn list * trait_defn list * expr
