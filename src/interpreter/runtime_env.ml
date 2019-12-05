open Ast.Ast_types
open Core
open Result

type threadID = int
type address = int

let string_of_address address = Int.to_string address
let string_of_thread_id thread_id = Int.to_string thread_id
let compare_thread_ids tid1 tid2 = tid1 - tid2
let compare_addresses addr1 addr2 = addr1 - addr2

(* Values are irreducible expressions - these are stored on the stack along with any
   environment bindings. *)
type value =
  | NULL
  | REF       of address (* An reference is an address in the heap *)
  | INT       of int
  | CLOSURE   of code * env
  | THREAD_ID of threadID

(* this final value type is placed as a marker on the stack to determine the thread that
   the current thread is waiting on *)
and instruction =
  | PUSH              of value
  | BIND              of Var_name.t
  (* expects the set value to be on top of the stack. Creates a new binding on stack *)
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
  | APPLY
  (* function application - takes top two elements of stack (value, closure) in that
     order and applies value to a closure *)
  | CONSTRUCTOR       of Class_name.t (* Creates a new object on the stack *)
  | SPAWN             of code

(* This spawns a new thread with the given instruction list *)
and code = instruction list

and binding = Var_name.t * value

(* Code is used interchangeably when referring to a list of instructions *)
and env = binding list

(* An environment binds variables to values *)

type env_or_value = Env of env | V of value
type stack = env_or_value list

(* We can push both values and environments on the stack *)

type obj = {class_name: Class_name.t; mutable fields: (Field_name.t * value) list}

(* Note we tag each object with its class name (could be used in future if subtyping
   introduced into language) *)

type heap = (address * obj) list

(* A heap maps addresses to objects *)

(* Note each thread has a local stack, but heap is global *)
type thread = TThread of threadID * code * stack
type thread_pool = thread list

(*********** STACK helper methods **************)

(* Goes through stack and flattens all the environments into one big environment - makes
   it easy to search for bindings on the stack *)
let get_env_of_stack stack =
  List.fold ~init:[] ~f:(fun acc -> function V _ -> acc | Env env -> acc @ env) stack

let stack_lookup stack var_name =
  (* return the first binding, as this corresponds to most inner scope *)
  get_env_of_stack stack
  |> fun env ->
  List.filter ~f:(fun (name, _) -> name = var_name) env
  |> function
  | []                 ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: variable %s not found in stack"
              (Var_name.to_string var_name)))
  | (_, value) :: _ -> Ok value

(* In future, could be even cleverer and only return the free vars referenced in the code *)
let get_free_var_bindings _ stack =
  let env_with_possible_duplicates =
    get_env_of_stack stack
    (* This returns all bindings in the stack, but we only want most recent *) in
  List.fold ~init:[]
    ~f:(fun env_so_far (var_name, value) ->
      if
        List.exists ~f:(fun (name, _) -> name = var_name) env_so_far
        (* Check if variable has already been bound *)
      then env_so_far (* we ignore all duplicate bindings *)
      else (var_name, value) :: env_so_far)
    env_with_possible_duplicates

let is_var_in_env var_name env =
  List.filter ~f:(fun (name, _) -> name = var_name) env
  (* this gets only the bindings for var_name *)
  |> function [] -> false | _ -> true

let rec replace_var_in_env var_name new_value = function
  | []                      -> []
  | (name, value) :: env ->
      if var_name = name then (var_name, new_value) :: env
      else (name, value) :: replace_var_in_env var_name new_value env

(* Replace the most recent binding of the var in the stack *)
let rec stack_set_var stack var_name value =
  match stack with
  | []                  -> [Env [(var_name, value)]]
  | V v :: stk     -> V v :: stack_set_var stk var_name value
  (* Skip over values *)
  | Env env :: stk ->
      if is_var_in_env var_name env then
        (*If present in this env, update the value, otherwise skip over this env and look
          in a later env *)
        Env (replace_var_in_env var_name value env) :: stk
      else Env env :: stack_set_var stk var_name value

(*********** HEAP helper methods **************)

(* This returns the address of the object as well as the updated heap*)
let create_obj heap class_name =
  let new_addr =
    List.fold ~init:0 ~f:(fun max (addr, _) -> if max < addr then addr else max) heap + 1
    (* assign the next free address in ascending order *) in
  let new_obj = {class_name; fields= []} in
  (new_addr, (new_addr, new_obj) :: heap)

let heap_look_up_object heap address =
  List.filter ~f:(fun (addr, _) -> addr = address) heap
  (* get object corresponding to address *)
  |> function
  | [(_, obj)] -> Ok obj
  | _                 ->
      Error
        (Error.of_string
           (Fmt.str
              "Runtime error: Could not find unique object corresponding to address: %s"
              (string_of_address address)))

let heap_lookup_field heap address field_name =
  heap_look_up_object heap address
  >>= fun obj ->
  List.filter ~f:(fun (name, _) -> field_name = name) obj.fields
  (* get field corresponding to field_name*)
  |> function
  | [(_, value)] -> Ok value
  | _                   ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: Could not find unique field corresponding to %s"
              (Field_name.to_string field_name)))

let heap_set_field heap address field_name new_value =
  let rec object_set_field field_name new_value = function
    | []                        -> [(field_name, new_value)]
    | (name, old_val) :: obj ->
        if name = field_name then (field_name, new_value) :: obj
        else (name, old_val) :: object_set_field field_name new_value obj in
  heap_look_up_object heap address
  >>| fun obj ->
  let new_fields = object_set_field field_name new_value obj.fields in
  obj.fields <- new_fields ;
  heap

(*********** THREAD helper methods **************)

let spawn_thread thread_pool code stack =
  let new_thread_id =
    List.fold ~init:0
      ~f:(fun max (TThread (id, _, _)) -> if max < id then id else max)
      thread_pool
    + 1
    (* assign the next free thread id in ascending order *) in
  (new_thread_id, TThread (new_thread_id, code, stack) :: thread_pool)

let init_thread_pool code stack =
  spawn_thread [] code stack |> fun (_, thread_pool) -> thread_pool

let remove_thread thread_id thread_pool =
  List.filter ~f:(fun (TThread (tid, _, _)) -> not (tid = thread_id)) thread_pool

let replace_thread (TThread (thread_id, instructions, stack)) thread_pool =
  TThread (thread_id, instructions, stack) :: remove_thread thread_id thread_pool

let get_thread thread_id thread_pool =
  List.filter ~f:(fun (TThread (tid, _, _)) -> tid = thread_id) thread_pool
  |> function
  | []              ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: Thread with id: %s not present.@."
              (string_of_thread_id thread_id)))
  | [thread] -> Ok thread
  | _               ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: Duplicate threads with id: %s  present.@."
              (string_of_thread_id thread_id)))
