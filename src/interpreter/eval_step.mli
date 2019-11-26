open Runtime_env
open Core

val eval_step : thread_pool -> heap -> threadID -> (thread_pool * heap, Error.t) result
(** Note we use small-step semantics *)
