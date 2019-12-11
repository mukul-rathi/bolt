open Core
open Runtime_env
open Pprint_execution_state
open Eval_step
open Scheduler
open Result
open Compile_program

(* This driver also prints out the state of the execution (using formatter ppf) *)
let rec verbose_driver ppf ~step_number state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, [], [V v])] ->
      pprint_execution_state ppf ~step_number thread_pool heap None ;
      Ok v (* Return the value on the top of the stack *)
  | [TThread (_, [], _)]     ->
      (* Bad finish state *)
      Error
        (Error.of_string
           "Runtime error: execution should finish with one thread and one value left on its stack")
  | _                        ->
      let scheduled_thread_id = schedule_thread Random thread_pool heap in
      pprint_execution_state ppf ~step_number thread_pool heap (Some scheduled_thread_id) ;
      verbose_driver ppf ~step_number:(step_number + 1)
        (eval_step thread_pool heap ~scheduled_thread_id)

let rec driver state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, [], [V v])] -> Ok v (* Return the value on the stack *)
  | [TThread (_, [], _)]     ->
      (* If there isn't a single value left on the stack then we've got a bug with the
         PUSH/POP operations on the stack *)
      Error
        (Error.of_string
           "Runtime error: execution should finish with one thread and one value left on its stack")
  | _                        ->
      let scheduled_thread_id = schedule_thread Random thread_pool heap in
      driver (eval_step thread_pool heap ~scheduled_thread_id)

let execute_program program ~print_execution =
  compile_program program
  >>= fun (code, stack, heap) ->
  let thread_pool = init_thread_pool code stack in
  let initial_state = (thread_pool, heap) in
  match print_execution with
  (* If we've been passed in a formatter, pretty print the states of execution, else just
     execute normally *)
  | Some ppf -> verbose_driver ppf ~step_number:0 (Ok initial_state)
  | None -> driver (Ok initial_state)

let print_result ppf value = Fmt.pf ppf "Output: %s@." (string_of_value value)
