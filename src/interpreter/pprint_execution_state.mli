(** This module pretty prints the execution state at a given step of the execution.*)

open Runtime_env

val pprint_execution_state :
     Format.formatter
  -> step_number:int
  -> thread_pool
  -> heap
  -> threadID option
     (** Pass in the scheduled thread id - if not present then we assume we're in the
         output state *)
  -> unit

val string_of_value : value -> string
val pprint_initial_state : Format.formatter -> bytecode * stack * heap -> unit
