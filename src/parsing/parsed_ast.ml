open Ast_types

(** Possible types of executable expressions - note we pass in the location of the start
    token to provide useful debugging information - which line + position the parsing
    errors occurred *)
type expr =
  | Integer     of loc * int
  | Variable    of loc * Var_name.t
  | Lambda      of loc * Var_name.t * type_expr * expr
      (** argument_variable, argument_type and body expression of function *)
  | App         of loc * expr * expr
  | Seq         of loc * expr list
  | Let         of loc * Var_name.t * expr * expr
  | ObjField    of loc * Var_name.t * Field_name.t
  | Assign      of loc * Var_name.t * Field_name.t * expr
  | Constructor of loc * Class_name.t * constructor_arg list
  | Consume     of loc * expr
  | FinishAsync of loc * expr * expr * expr

and constructor_arg = ConstructorArg of Field_name.t * expr

(** Each bolt program defines the classes, followed by the traits, followed by the
    expression to execute. *)
type program = Prog of class_defn list * trait_defn list * expr
