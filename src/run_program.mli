(** This is the entry point for execution of Bolt programs. *)

val run_program :
     ?should_pprint_past:bool
  -> ?should_pprint_tast:bool
  -> ?should_pprint_dast:bool
  -> ?should_pprint_last:bool
  -> ?optional_filename:string
  -> Lexing.lexbuf
  -> unit
