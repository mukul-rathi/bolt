open Ast_types
open Core

type threadID
(** Abstract data types for thread ID and heap address *)

type address

val string_of_address : address -> string
val string_of_thread_id : threadID -> string
val compare_thread_ids : threadID -> threadID -> int
val compare_addresses : address -> address -> int

type value =
  | NULL
  | ADDR      of address
  | INT       of int
  | CLOSURE   of code * env
  | THREAD_ID of threadID
      (** this final value type is placed as a marker to determine the thread that the
          current thread is waiting on *)

and instruction =
  | PUSH              of value
  | BIND              of Var_name.t
      (** expects the set value to be on top of the stack. Creates a new binding on stack *)
  | BLOCKED
  | MK_CLOSURE        of code  (** pushes a closure onto the stack *)
  | STACK_LOOKUP      of Var_name.t
  | STACK_SET         of Var_name.t
      (** expects the set value to be on top of the stack. Also note - this updates the
          most recent binding for the variable, unlike BIND which adds a new binding *)
  | HEAP_FIELD_LOOKUP of Field_name.t
      (** note this expects the object's Addr on top of the stack *)
  | HEAP_FIELD_SET    of Field_name.t
      (** note this expects [Addr, Value] as top two elements of the stack *)
  | SWAP  (** Swaps the top two elements on the stack *)
  | POP
  | APPLY  (** function application - applies value to a closure *)
  | CONSTRUCTOR       of Class_name.t  (** Creates a new object on the stack *)
  | SPAWN             of code  (** This spawns a new thread which executes the code *)

and code = instruction list

and binding = Var_name.t * value

and env = binding list

type obj = {class_name: Class_name.t; mutable fields: (Field_name.t * value) list}
type env_or_value = Env of env | V of value

type stack = env_or_value list
(** Note each thread has a local stack, but heap is global *)

type heap = (address * obj) list
type thread = TThread of threadID * code * stack
type thread_pool = thread list

val init_thread_pool : code -> stack -> thread_pool
(** Arguments = a compiled program - i.e. the instructions and the initial stack -
    initialises a thread pool in order to begin execution *)

val create_obj : heap -> Class_name.t -> address * heap
(** This returns the address of the object as well as the updated heap*)

val get_free_var_bindings : code -> stack -> env
val stack_lookup : stack -> Var_name.t -> value Or_error.t
val stack_set_var : stack -> Var_name.t -> value -> stack
val spawn_thread : thread_pool -> code -> stack -> threadID * thread_pool
val heap_lookup_field : heap -> address -> Field_name.t -> value Or_error.t
val heap_set_field : heap -> address -> Field_name.t -> value -> heap Or_error.t
val remove_thread : threadID -> thread_pool -> thread_pool
val replace_thread : thread -> thread_pool -> thread_pool
val get_thread : threadID -> thread_pool -> thread Or_error.t
