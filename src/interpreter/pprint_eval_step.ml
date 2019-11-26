open Runtime_env
open Core
open Ast_types

let indent_space = "   "

let rec string_of_value = function
  | NULL                -> "NULL"
  | ADDR address        -> Fmt.str "Address: %s" (string_of_address address)
  | INT i               -> Fmt.str "Int: %d" i
  | CLOSURE (code, env) ->
      Fmt.str "Closure: ( Body: [ %s \n  ] Env: [ %s ])" (string_of_code code)
        (string_of_env env)
  | THREAD_ID thread_id -> Fmt.str "Thread ID: %s" (string_of_thread_id thread_id)

and string_of_env env =
  String.concat ~sep:","
    (List.map
       ~f:(fun (var_name, value) ->
         Fmt.str "%s -> %s" (Var_name.to_string var_name) (string_of_value value))
       env)

and string_of_code code = String.concat ~sep:";" (List.map ~f:string_of_instruction code)

and string_of_instruction = function
  | PUSH value                   -> Fmt.str "PUSH(%s)" (string_of_value value)
  | BIND var_name                -> Fmt.str "BIND(%s)" (Var_name.to_string var_name)
  | BLOCKED                      -> "BLOCKED"
  | MK_CLOSURE code              -> Fmt.str "MK_CLOSURE(%s)" (string_of_code code)
  | STACK_LOOKUP var_name        -> Fmt.str "STACK_LOOKUP(%s)"
                                      (Var_name.to_string var_name)
  | STACK_SET var_name           -> Fmt.str "STACK_SET(%s)" (Var_name.to_string var_name)
  | HEAP_FIELD_LOOKUP field_name ->
      Fmt.str "HEAP_FIELD_LOOKUP(%s)" (Field_name.to_string field_name)
  | HEAP_FIELD_SET field_name    ->
      Fmt.str "HEAP_FIELD_SET(%s)" (Field_name.to_string field_name)
  | SWAP                         -> "SWAP"
  | POP                          -> "POP"
  | APPLY                        -> "APPLY"
  | CONSTRUCTOR class_name       -> Fmt.str "CONSTRUCTOR(%s)"
                                      (Class_name.to_string class_name)
  | SPAWN code                   -> Fmt.str "SPAWN(%s)" (string_of_code code)

let string_of_obj obj =
  let string_of_fields fields =
    String.concat ~sep:";"
      (List.map
         ~f:(fun (field_name, value) ->
           Fmt.str "%s: %s" (Field_name.to_string field_name) (string_of_value value))
         fields) in
  Fmt.str "{ Class_name: %s, Fields: { %s } }"
    (Class_name.to_string obj.class_name)
    (string_of_fields obj.fields)

let string_of_heap heap =
  String.concat ~sep:","
    (List.map
       ~f:(fun (addr, obj) ->
         Fmt.str "%s |-> %s" (string_of_address addr) (string_of_obj obj))
       heap)

let string_of_stack stack =
  String.concat ~sep:","
    (List.map
       ~f:(function
         | V v     -> Fmt.str "Value: %s" (string_of_value v)
         | Env env -> Fmt.str "Env: [ %s ]" (string_of_env env))
       stack)

let pprint_thread ppf indent (TThread (thread_id, code, stack)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%sThread: %s@." indent (string_of_thread_id thread_id) ;
  Fmt.pf ppf "%sInstructions: [ %s ]@." new_indent (string_of_code code) ;
  Fmt.pf ppf "%sStack: [ %s ]@." new_indent (string_of_stack stack)

let pprint_eval_step ppf ~step_number thread_pool heap maybe_scheduled_thread_id =
  let indent = "└──" in
  ( match maybe_scheduled_thread_id with
  | Some scheduled_thread_id ->
      Fmt.pf ppf "----- Step %d - scheduled thread : %s-----@." step_number
        (string_of_thread_id scheduled_thread_id)
  | None                     -> Fmt.pf ppf "----- Step %d - OUTPUT -----@." step_number
  ) ;
  Fmt.pf ppf "Threads:@." ;
  List.iter ~f:(pprint_thread ppf indent) thread_pool ;
  Fmt.pf ppf "Heap: [ %s ]@." (string_of_heap heap) ;
  Fmt.pf ppf "------------------------------------------@."
