(** This module implements the scheduler, which takes in the state of the execution
    (thread pool and heap) and picks a thread to execute *)

open Runtime_env

(** Options to select type of scheduling algorithm used *)
type schedule = Random

val schedule_thread : schedule -> thread_pool -> heap -> threadID
