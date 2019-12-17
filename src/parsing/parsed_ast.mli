open Ast.Ast_types

(** Possible types of executable expressions - note we pass in the location of the start
    token to provide useful debugging information - which line + position the parsing
    errors occurred *)
type expr =
  | Integer     of loc * int
  | Variable    of loc * Var_name.t
  | App         of loc * Function_name.t * expr list
  | Block       of loc * expr list
  | Let         of loc * Var_name.t * expr  (** binds variable to expression *)
  | ObjField    of loc * Var_name.t * Field_name.t  (** read as x.f *)
  | ObjMethod   of loc * Var_name.t * Function_name.t * expr list  (** read as x.m(args) *)
  | Assign      of loc * Var_name.t * Field_name.t * expr  (** read as x.f := e *)
  | Constructor of loc * Class_name.t * constructor_arg list
  | Consume     of loc * expr
  | FinishAsync of loc * expr * expr * expr
      (** first async expr, second async expr, next expr after async exection completed *)

and constructor_arg = ConstructorArg of Field_name.t * expr  (** read as (f: ___) *)

(** Function defn contains the function name, return type, the list of params, and the
    body expr of the function *)
type function_defn = TFunction of Function_name.t * type_expr * param list * expr

(** Class definitions consist of the class name, the trait it is implementing (with
    capability associated) and the fields and methods in the class *)
type class_defn =
  | TClass of Class_name.t * cap_trait * field_defn list * function_defn list

(** Each bolt program defines the classes, followed by the traits, followed by functions,
    followed by the expression to execute. *)
type program = Prog of class_defn list * trait_defn list * function_defn list * expr
