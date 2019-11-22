open Ast_types

(** Possible types of executable expressions *)
type expr =
  | Integer     of loc * int
  | Variable    of loc * Var_name.t
  | Lambda      of loc * Var_name.t * type_expr * expr
  | App         of loc * expr * expr
  | Seq         of loc * expr list
  | Let         of loc * Var_name.t * expr * expr
  | ObjField    of loc * Var_name.t * Field_name.t
  | Assign      of loc * Var_name.t * Field_name.t * expr
  | Constructor of loc * Class_name.t * constructor_arg list
  | Consume     of loc * Var_name.t
  | FinishAsync of loc * expr * expr * expr

and constructor_arg = ConstructorArg of Field_name.t * expr

(** Each bolt program defines the classes, followed by the traits, followed by the
    expression to execute. *)
type program = Prog of class_defn list * trait_defn list * expr
