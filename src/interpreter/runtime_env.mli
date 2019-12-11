(** This module defines the runtime environment of the interpreter, from the instructions
    and values to the definition of stacks, heaps and threads. It also contains a list of
    helper functions to manipulate the said types defined here. *)

open Ast.Ast_types
open Core

(** Abstract data types for thread ID and heap address *)

type threadID
type address

(** Helper functions to compare and print to string *)

val string_of_address : address -> string
val string_of_thread_id : threadID -> string
val compare_thread_ids : threadID -> threadID -> int
val compare_addresses : address -> address -> int

(** Values are irreducible expressions - these are stored on the stack along with any
    environment bindings. *)
type value =
  | NULL
  | REF       of address  (** An reference is an address in the heap *)
  | INT       of int
  | CLOSURE   of bytecode * env
  | THREAD_ID of threadID
      (** this final value type is placed as a marker on the stack to determine the thread
          that the current thread is waiting on *)

and instruction =
  | PUSH              of value
  | BIND              of Var_name.t
      (** expects the set value to be on top of the stack. Creates a new binding on stack *)
  | BLOCKED
  | MK_CLOSURE        of bytecode  (** pushes a closure onto the stack *)
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
  | APPLY
      (** function application - takes top two elements of stack (value, closure) in that
          order and applies value to a closure *)
  | CONSTRUCTOR       of Class_name.t  (** Creates a new object on the stack *)
  | SPAWN             of bytecode  (** This spawns a new thread with the given instruction
                                       list *)

and bytecode = instruction list

and binding = Var_name.t * value
(** Bytecode is used interchangeably when referring to a list of instructions *)

and env = binding list
(** An environment binds variables to values *)

type env_or_value = Env of env | V of value
type stack = env_or_value list

(** We can push both values and environments on the stack *)

type obj = {class_name: Class_name.t; mutable fields: (Field_name.t * value) list}

(** Note we tag each object with its class name (could be used in future if subtyping
    introduced into language) *)

type heap = (address * obj) list
(** A heap maps addresses to objects *)

(** Note each thread has a local stack, but heap is global *)
type thread = TThread of threadID * bytecode * stack

type thread_pool = thread list

val init_thread_pool : bytecode -> stack -> thread_pool
(** Arguments = the instructions and the initial stack of a compiled program - initialises
    a thread pool in order to begin execution *)

val create_obj : heap -> Class_name.t -> address * heap
(** This returns the address of the new object as well as the updated heap *)

(** Helper methods for stack *)

val get_free_var_bindings : bytecode -> stack -> env
val stack_lookup : stack -> Var_name.t -> value Or_error.t
val stack_set_var : stack -> Var_name.t -> value -> stack

val heap_lookup_field : heap -> address -> Field_name.t -> value Or_error.t
(** Helper methods for the heap *)

val heap_set_field : heap -> address -> Field_name.t -> value -> heap Or_error.t

(** Helper methods for threads *)

val spawn_thread : thread_pool -> bytecode -> stack -> threadID * thread_pool
val get_thread : threadID -> thread_pool -> thread Or_error.t
val remove_thread : threadID -> thread_pool -> thread_pool
val replace_thread : thread -> thread_pool -> thread_pool
