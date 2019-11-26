open Runtime_env

val pprint_eval_step :
     Format.formatter
  -> step_number:int
  -> thread_pool
  -> heap
  -> threadID option (* Prints scheduled thread if given, or just output of state *)
  -> unit

val string_of_value : value -> string
