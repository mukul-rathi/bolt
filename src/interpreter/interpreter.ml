open Core
open Runtime_env
open Pprint_eval_step
open Eval_step
open Scheduler
open Result

let rec verbose_driver ppf ~step_number state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, Value v, _)] -> Ok (Value v)
  | _                                    ->
      pprint_eval_step ppf ~step_number thread_pool heap ;
      let threadID = schedule_thread Random thread_pool heap in
      verbose_driver ppf ~step_number:(step_number + 1)
        (eval_step thread_pool heap threadID)

let rec driver state =
  state
  >>= fun (thread_pool, heap) ->
  match thread_pool with
  | [TThread (_, Value v, _)] -> Ok (Value v)
  | _                                    ->
      let threadID = schedule_thread Random thread_pool heap in
      driver (eval_step thread_pool heap threadID)

let run_program (Typed_ast.Prog (_, _, expr)) ~print_execution =
  let init_state = init_runtime_env expr in
  ( match print_execution with
  | Some ppf -> verbose_driver ppf ~step_number:0 (Ok init_state)
  | None     -> driver (Ok init_state) )
  >>= function
  | Value v -> Ok v | _ -> Error (Error.of_string (Fmt.str "Runtime error: got stuck."))

let print_result ppf value = Fmt.pf ppf "Output: %s@." (string_of_value value)
