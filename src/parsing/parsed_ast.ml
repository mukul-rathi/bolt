open Ast.Ast_types

(* Possible types of executable expressions - note we pass in the location of the start
   token to provide useful debugging information - which line + position the parsing
   errors occurred *)
type expr =
  | Unit        of loc
  | Integer     of loc * int
  | Boolean     of loc * bool
  | Variable    of loc * Var_name.t
  | App         of loc * Function_name.t * expr list
  | Block       of loc * expr list
  | Let         of loc * Var_name.t * expr
  | ObjField    of loc * Var_name.t * Field_name.t
  | ObjMethod   of loc * Var_name.t * Function_name.t * expr list
  | Assign      of loc * Var_name.t * Field_name.t * expr
  | Constructor of loc * Class_name.t * constructor_arg list
  | Consume     of loc * expr
  | FinishAsync of loc * expr * expr * expr
  | If          of loc * expr * expr * expr
  | While       of loc * expr * expr
  | BinOp       of loc * bin_op * expr * expr
  | UnOp        of loc * un_op * expr

and constructor_arg = ConstructorArg of Field_name.t * expr

type function_defn = TFunction of Function_name.t * type_expr * param list * expr

type class_defn =
  | TClass of Class_name.t * cap_trait * field_defn list * function_defn list

(* Each bolt program defines the classes, followed by the traits, followed by functions,
   followed by the expression to execute. *)
type program = Prog of class_defn list * trait_defn list * function_defn list * expr
