(** This is the entry point for execution of Bolt programs. *)

val run_program :
     Lexing.lexbuf
  -> should_pprint_past:bool
  -> should_pprint_tast:bool
  -> check_data_races:bool
  -> print_execution:bool
  -> unit
  -> unit
