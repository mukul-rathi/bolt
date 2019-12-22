(** The typed AST consists of the original AST but augmented with type information. *)

open Ast.Ast_types

(** Similar to Parsed AST, only we add an extra type_expr annotation to denote the overall
    type of the expression. *)
type expr =
  | Unit        of loc  (** no need for type_expr annotation as obviously TEUnit *)
  | Integer     of loc * int  (** no need for type_expr annotation as obviously TEInt *)
  | Boolean     of loc * bool  (** no need for type_expr annotation as obviously TEBool *)
  | Variable    of loc * type_expr * Var_name.t
  | App         of loc * type_expr * Function_name.t * expr list
  | Block       of loc * type_expr * expr list  (** type is of the final expr in block *)
  | Let         of loc * type_expr * Var_name.t * expr
  | ObjField    of loc * type_expr * Var_name.t * type_expr * Field_name.t
      (** First type is of the overall expr x.f, second is the type of the obj x *)
  | ObjMethod   of loc * type_expr * Var_name.t * type_expr * Function_name.t * expr list
      (** First type is of the overall expr x.f, second is the type of the obj x *)
  | Assign      of loc * type_expr * Var_name.t * type_expr * Field_name.t * expr
      (** First type is of the expr, second is the type of the obj *)
  | Constructor of loc * type_expr * Class_name.t * constructor_arg list
  | Consume     of loc * type_expr * expr  (** type is that of the expr being consumed *)
  | FinishAsync of loc * type_expr * expr * expr
      (** overall type is that of the first async expr - since that's the expression that
          continued executing on that thread, not the forked one *)
  | If          of loc * type_expr * expr * expr * expr
  | While       of loc * expr * expr
      (** While ___ do ___ ; - no need for type_expr annotation as type of a loop is
          TEUnit *)
  | BinOp       of loc * type_expr * bin_op * expr * expr
  | UnOp        of loc * type_expr * un_op * expr

and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

type function_defn = TFunction of Function_name.t * type_expr * param list * expr

(** Class definitions consist of the class name, the trait it is implementing (with
    capability associated) and the fields and methods in the class *)
type class_defn =
  | TClass of Class_name.t * cap_trait * field_defn list * function_defn list

type program = Prog of class_defn list * trait_defn list * function_defn list * expr
