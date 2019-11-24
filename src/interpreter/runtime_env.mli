open Ast_types

type threadID
(** Abstract data types for thread ID and heap address *)

type address

val string_of_address : address -> string
val string_of_thread_id : threadID -> string

type value = NULL | ADDR of address | INT of int | FUN of (value -> expr)

and expr =
  | TypedExpr of Typed_ast.expr
  | Value     of value
  | Blocked   of threadID
  | Seq       of expr list

type obj = {class_name: Class_name.t; fields: (Field_name.t * value) list}

type stack = (Var_name.t * value) list
(** Note each thread has a local stack, but heap is global *)

type heap = (address * obj) list
type thread = TThread of threadID * expr * stack
type thread_pool = thread list

val init_runtime_env : Typed_ast.expr -> thread_pool * heap

val create_obj : heap -> Class_name.t -> (Field_name.t * value) list -> address * heap
(** This returns the address of the object as well as the updated heap*)

val spawn_thread : thread_pool -> expr -> stack -> thread_pool
