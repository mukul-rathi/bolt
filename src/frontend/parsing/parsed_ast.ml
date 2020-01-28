open Ast.Ast_types

type identifier = Variable of Var_name.t | ObjField of Var_name.t * Field_name.t

(* Possible types of executable expressions - note we pass in the location of the start
   token to provide useful debugging information - which line + position the parsing
   errors occurred *)
type expr =
  | Integer     of loc * int
  | Boolean     of loc * bool
  | Identifier  of loc * identifier
  | Block       of loc * expr list
  | Constructor of loc * Class_name.t * constructor_arg list
  | Let         of loc * Var_name.t * expr (* binds variable to expression *)
  | Assign      of loc * identifier * expr
  | Consume     of loc * identifier
  | MethodApp   of loc * Var_name.t * Method_name.t * expr list (* read as x.m(args) *)
  | FunctionApp of loc * Function_name.t * expr list
  | Printf      of loc * string * expr list
  | FinishAsync of loc * expr list * expr
  (* a list async exprs and the current thread's expr *)
  | If          of loc * expr * expr * expr  (** If ___ then ___ else ___ *)
  | While       of loc * expr * expr  (** While ___ do ___ *)
  | For         of loc * expr * expr * expr * expr
  (* For(init_expr; cond_expr ; step_expr) body_expr *)
  | BinOp       of loc * bin_op * expr * expr
  | UnOp        of loc * un_op * expr

and constructor_arg = ConstructorArg of Field_name.t * expr

type function_defn = TFunction of Function_name.t * type_expr * param list * expr

type method_defn =
  | TMethod of Method_name.t * type_expr * param list * Region_name.t list * expr

type class_defn =
  | TClass of Class_name.t * region list * field_defn list * method_defn list

(* Each bolt program defines the classes, followed by the traits, followed by functions,
   followed by the expression to execute. *)
type program = Prog of class_defn list * function_defn list * expr
