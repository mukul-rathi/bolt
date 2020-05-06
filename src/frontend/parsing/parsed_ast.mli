(** This module specifies the structure of the parsed AST *)

open Ast.Ast_types

type identifier = Variable of Var_name.t | ObjField of Var_name.t * Field_name.t

(** Possible executable expressions - note we pass in the location of the start token to
    provide useful debugging information - which line + position the parsing errors
    occurred *)
type expr =
  | Integer     of loc * int
  | Boolean     of loc * bool
  | Identifier  of loc * identifier
  | Constructor of loc * Class_name.t * type_expr option * constructor_arg list
      (** optional type-parameter *)
  | Let         of loc * type_expr option * Var_name.t * expr
      (** binds variable to expression (optional type annotation) *)
  | Assign      of loc * identifier * expr
  | Consume     of loc * identifier
  | MethodApp   of loc * Var_name.t * Method_name.t * expr list  (** read as x.m(args) *)
  | FunctionApp of loc * Function_name.t * expr list
  | Printf      of loc * string * expr list
  | FinishAsync of loc * async_expr list * block_expr
      (** a list async exprs and the current thread's expr *)
  | If          of loc * expr * block_expr * block_expr  (** If ___ then ___ else ___ *)
  | While       of loc * expr * block_expr  (** While ___ do ___ *)
  | For         of loc * expr * expr * expr * block_expr
      (** For(init_expr; cond_expr ; step_expr) body_expr *)
  | BinOp       of loc * bin_op * expr * expr
  | UnOp        of loc * un_op * expr

and block_expr = Block of loc * expr list

and async_expr = AsyncExpr of block_expr

and constructor_arg = ConstructorArg of Field_name.t * expr

(** Function defn consists of the function name, return type (and whether it returns a
    borrowed ref), the list of params, and the body expr of the function *)
type function_defn =
  | TFunction of
      Function_name.t * borrowed_ref option * type_expr * param list * block_expr

(** Method defn consists the method name, return type (and whether it returns a borrowed
    ref), the list of params, the capabilities used and the body expr of the function *)
type method_defn =
  | TMethod of
      Method_name.t
      * borrowed_ref option
      * type_expr
      * param list
      * Capability_name.t list
      * block_expr

(** Class definitions consist of the class name and optionally specifying if generic and
    if it inherits from another class, its capabilities and the fields and methods in the
    class *)
type class_defn =
  | TClass of
      Class_name.t
      * generic_type option
      * Class_name.t option
      * capability list
      * field_defn list
      * method_defn list

(** Each bolt program defines the classes,followed by functions, followed by the main
    expression block to execute. *)
type program = Prog of class_defn list * function_defn list * block_expr
