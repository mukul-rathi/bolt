(** We use small-step semantics when executing the interpreter. This module takes in a
    state (thread pool and heap) and the scheduled thread's ID and then executes a single
    step of execution (using top instruction in the code stack of that thread, and
    returns the updated state if successful *)

open Runtime_env
open Core

val eval_step :
  thread_pool -> heap -> scheduled_thread_id:threadID -> (thread_pool * heap) Or_error.t
