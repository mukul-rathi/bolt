open Ast_types

type expr =
  | Integer     of int (* no need for type_expr annotation as obviously TEInt *)
  | Variable    of type_expr * Var_name.t
  | Lambda      of type_expr * Var_name.t * type_expr * expr
  | App         of type_expr * expr * expr
  | Seq         of type_expr * expr list
  | Let         of type_expr * Var_name.t * expr * expr
  | ObjField    of type_expr * Var_name.t * Field_name.t
  | Assign      of type_expr * Var_name.t * Field_name.t * expr
  | Constructor of type_expr * Class_name.t * constructor_arg list
  | Consume     of type_expr * expr
  | FinishAsync of type_expr * expr * expr * expr

and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

type program = Prog of class_defn list * trait_defn list * expr
