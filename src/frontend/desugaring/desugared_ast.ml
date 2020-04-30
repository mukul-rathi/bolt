(* The desugared AST consists of the typed AST but simplified + pre-processed to make
   analysis easier in later stages of the pipeline.

   E.g there is no variable shadowing (variables have been given unique monikers)

   The idea is that this decouples the data-race type-checking from any future syntactic
   changes and extensions to the language. E.g. in future there may be support for
   unstructured spawning and synchronisation of threads. *)

open Ast.Ast_types

(* capabilities and modes are associated with this identifier *)
type identifier =
  | Variable of type_expr * Var_name.t * capability list * borrowed_ref option
  | ObjField of
      Class_name.t (* class of the object*)
      * Var_name.t
      * type_expr
      * Field_name.t (*type of field *)
      * capability list
      * borrowed_ref option

let string_of_id id =
  match id with
  | Variable (_, var_name, _, maybe_borrowed) ->
      Fmt.str "%sVariable: %s"
        (string_of_maybe_borrowed_ref maybe_borrowed)
        (Var_name.to_string var_name)
  | ObjField (obj_class, var_name, _, field_name, _, maybe_borrowed) ->
      Fmt.str "%sObjfield: (Class: %s) %s.%s"
        (string_of_maybe_borrowed_ref maybe_borrowed)
        (Class_name.to_string obj_class)
        (Var_name.to_string var_name)
        (Field_name.to_string field_name)

(* an object variable and associated types and capabilities *)
type obj_var_and_capabilities = Var_name.t * Class_name.t * capability list

type expr =
  | Integer     of loc * int (* no need for type_expr annotation as obviously TEInt *)
  | Boolean     of loc * bool (* no need for type_expr annotation as obviously TEBool *)
  | Identifier  of loc * identifier (* Type information associated with identifier *)
  | BlockExpr   of loc * block_expr (* used to interconvert with block expr *)
  | Constructor of loc * type_expr * Class_name.t * constructor_arg list
  | Let         of loc * type_expr * Var_name.t * expr
  | Assign      of loc * type_expr * identifier * expr
  | Consume     of loc * identifier (* Type is associated with the identifier *)
  | MethodApp   of
      loc
      * type_expr
      * Var_name.t
      * capability list
      * Class_name.t
      * Method_name.t
      * expr list
  (* we track the allowed capabilities of the variable calling the method *)
  | FunctionApp of loc * type_expr * Function_name.t * expr list
  | Printf      of loc * string * expr list
  (* no need for type_expr annotation as obviously TEVoid *)
  | FinishAsync of
      loc * type_expr * async_expr list * obj_var_and_capabilities list * block_expr
  (* overall type is that of the expr on the current thread - since forked exprs' values
     are ignored. [ obj_var_and_capabilities list] is a list of free object variables in
     the block expression and their associated types and capabilities *)
  | If          of loc * type_expr * expr * block_expr * block_expr
  (* If ___ then ___ else ___ - type is that of the branch exprs *)
  | While       of loc * expr * block_expr
  (* While ___ do ___ ; - no need for type_expr annotation as type of a loop is TEVoid *)
  | BinOp       of loc * type_expr * bin_op * expr * expr
  | UnOp        of loc * type_expr * un_op * expr

and block_expr = Block of loc * type_expr * expr list

(* type is of the final expr in block *)

(* Constructor arg consists of a field and the expression being assigned to it (annotated
   with the type of the expression) *)
and constructor_arg = ConstructorArg of type_expr * Field_name.t * expr

(* Async exprs have a precomputed list of their free object variables and their associated
   classes (passed as arguments when they are spawned as thread) *)
and async_expr = AsyncExpr of obj_var_and_capabilities list * block_expr

(* Function defn consists of the function name, return type (and whether it returns a
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
      * capability list
      * block_expr

(* Class definitions consist of the class name and optionally if it inherits from another
   class, its capabilities and the fields and methods in the class *)
type class_defn =
  | TClass of
      Class_name.t
      * Class_name.t option
      * capability list
      * field_defn list
      * method_defn list

(* Each bolt program defines the classes,followed by functions, followed by the main
   expression block to execute. *)
type program = Prog of class_defn list * function_defn list * block_expr
