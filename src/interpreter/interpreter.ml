open Core
open Runtime_env
open Pprint_eval_step
open Eval_step
open Scheduler
open Result
open Compile_program

let rec verbose_driver ppf ~step_number state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, [], V v :: _)] ->
      pprint_eval_step ppf ~step_number thread_pool heap None ;
      Ok v (* Return the value on the top of the stack *)
  | _                                           ->
      let thread_id = schedule_thread Random thread_pool heap in
      pprint_eval_step ppf ~step_number thread_pool heap (Some thread_id) ;
      verbose_driver ppf ~step_number:(step_number + 1)
        (eval_step thread_pool heap thread_id)

let rec driver state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, [], [V v])] -> Ok v (* Return the value on the top of the stack *)
  | [TThread (_, [], _)]                ->
      Error
        (Error.of_string
           "Runtime error: execution should finish with one thread and one value left \
            on its stack")
  | _                                            ->
      let threadID = schedule_thread Random thread_pool heap in
      driver (eval_step thread_pool heap threadID)

let run_program program ~print_execution =
  compile_program program
  >>= fun (code, stack, heap) ->
  let thread_pool = init_thread_pool code stack in
  let initial_state = (thread_pool, heap) in
  match print_execution with
  | Some ppf -> verbose_driver ppf ~step_number:0 (Ok initial_state)
  | None     -> driver (Ok initial_state)

let print_result ppf value = Fmt.pf ppf "Output: %s@." (string_of_value value)
