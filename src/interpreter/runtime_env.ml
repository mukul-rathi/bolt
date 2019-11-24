open Ast_types
open Core

type threadID = int
type address = int

let string_of_address address = Int.to_string address
let string_of_thread_id thread_id = Int.to_string thread_id

type value = NULL | ADDR of address | INT of int | FUN of (value -> expr)

and expr =
  | TypedExpr of Typed_ast.expr
  | Value     of value
  | Blocked   of threadID
  | Seq       of expr list

type obj = {class_name: Class_name.t; fields: (Field_name.t * value) list}

(* Note we only know this is a value of a function type but nothing else *)

(* Note each thread has a local stack, but heap is global *)
type stack = (Var_name.t * value) list
type heap = (address * obj) list
type thread = TThread of threadID * expr * stack
type thread_pool = thread list

(* This returns the address of the object as well as the updated heap*)
let create_obj heap class_name constructor_args =
  let new_addr =
    List.fold ~init:0 ~f:(fun max (addr, _) -> if max < addr then addr else max) heap
  in
  let new_obj = {class_name; fields= constructor_args} in
  (new_addr, (new_addr, new_obj) :: heap)

let spawn_thread thread_pool expr stack =
  let new_thread_id =
    List.fold ~init:0
      ~f:(fun max (TThread (id, _, _)) -> if max < id then id else max)
      thread_pool in
  TThread (new_thread_id, expr, stack) :: thread_pool

let init_runtime_env expr = (spawn_thread [] (TypedExpr expr) [], [])
