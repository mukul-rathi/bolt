(** The desugared AST consists of the typed AST but simplified.

    - for loops have been desugared into while loops.
    - Shadowing is not permitted, so variables have been given unique monikers *)

open Ast.Ast_types

type identifier =
  | Variable of type_expr * Var_name.t
  | ObjField of type_expr * Var_name.t * type_expr * Field_name.t
      (** first type is of the object, second is of field *)

type expr =
  | Unit        of loc  (** no need for type_expr annotation as obviously TEVoid *)
  | Integer     of loc * int  (** no need for type_expr annotation as obviously TEInt *)
  | Boolean     of loc * bool  (** no need for type_expr annotation as obviously TEBool *)
  | Identifier  of loc * identifier  (** Type information associated with identifier *)
  | Block       of loc * type_expr * expr list  (** type is of the final expr in block *)
  | Constructor of loc * type_expr * Class_name.t * constructor_arg list
  | Let         of loc * type_expr * Var_name.t * expr
  | Assign      of loc * type_expr * identifier * expr
  | Consume     of loc * identifier  (** Type is associated with the identifier *)
  | MethodApp   of loc * type_expr * Var_name.t * type_expr * Method_name.t * expr list
  | FunctionApp of loc * type_expr * Function_name.t * expr list
  | FinishAsync of loc * type_expr * expr list * expr
      (** overall type is that of the expr on the current thread - since forked exprs'
          values are ignored *)
  | If          of loc * type_expr * expr * expr * expr
      (** If ___ then ___ else ___ - type is that of the branch exprs *)
  | While       of loc * expr * expr
      (** While ___ do ___ ; - no need for type_expr annotation as type of a loop is
          TEVoid *)
  | BinOp       of loc * type_expr * bin_op * expr * expr
  | UnOp        of loc * type_expr * un_op * expr

and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

(** Function defn consists of the function name, return type, the list of params, and the
    body expr of the function *)
type function_defn = TFunction of Function_name.t * type_expr * param list * expr

(** Method defn consists the method name, return type, the list of params, the region
    affected and the body expr of the function *)
type method_defn =
  | TMethod of Method_name.t * type_expr * param list * Region_name.t list * expr

(** Class definitions consist of the class name, its capability regions and the fields and
    methods in the class *)
type class_defn =
  | TClass of Class_name.t * region list * field_defn list * method_defn list

(** Each bolt program defines the classes,followed by functions, followed by the main
    expression to execute. *)
type program = Prog of class_defn list * function_defn list * expr
