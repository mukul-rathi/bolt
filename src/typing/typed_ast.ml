open Ast_types

type expr =
  | Integer     of loc * int (* no need for type_expr annotation as obviously TEInt *)
  | Variable    of loc * type_expr * Var_name.t
  | Lambda      of loc * type_expr * Var_name.t * type_expr * expr
  | App         of loc * type_expr * expr * expr
  | Seq         of loc * type_expr * expr list
  | Let         of loc * type_expr * Var_name.t * expr * expr
  | ObjField    of loc * type_expr * Var_name.t * Field_name.t
  | Assign      of loc * type_expr * Var_name.t * Field_name.t * expr
  | Constructor of loc * type_expr * Class_name.t * constructor_arg list
  | Consume     of loc * type_expr * expr
  | FinishAsync of loc * type_expr * expr * expr * expr

and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

type program = Prog of class_defn list * trait_defn list * expr
