open Ast_types
open Core

type threadID = int
type address = int

let string_of_address address = Int.to_string address
let string_of_thread_id thread_id = Int.to_string thread_id

type value =
  | NULL
  | ADDR      of address
  | INT       of int
  | CLOSURE   of code * env
  | THREAD_ID of threadID

and instruction =
  | PUSH              of value
  | BIND              of Var_name.t
      (**expects the set value to be on top of the stack. Creates a new binding on stack *)
  | BLOCKED
  | MK_CLOSURE        of code (* pushes a closure onto the stack *)
  | STACK_LOOKUP      of Var_name.t
  | STACK_SET         of Var_name.t
  (* expects the set value to be on top of the stack. Also note - this updates the most
     recent binding for the variable, unlike BIND which adds a new binding *)
  | HEAP_FIELD_LOOKUP of Field_name.t
  (* note this expects the object's Addr on top of the stack *)
  | HEAP_FIELD_SET    of Field_name.t
  (* note this expects [Addr, Value] as top two elements of the stack *)
  | SWAP (* Swaps the top two elements on the stack *)
  | POP
  | APPLY (* function application - applies value to a closure *)
  | CONSTRUCTOR       of Class_name.t (* Creates a new object on the stack *)
  | SPAWN             of code

(* This spawns a new thread which starts with the *)
and code = instruction list

and binding = Var_name.t * value

and env = binding list

type obj = {class_name: Class_name.t; fields: (Field_name.t * value) list}
type env_or_value = Env of env | V of value
type stack = env_or_value list
type heap = (address * obj) list
type thread = TThread of threadID * code * stack
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
