(** This is the entry point for execution of Bolt programs. *)

val compile_program_ir :
     ?should_pprint_past:bool (** whether to print parsed AST *)
  -> ?should_pprint_tast:bool (** whether to print typed AST *)
  -> ?should_pprint_dast:bool (** whether to print desugared AST *)
  -> ?should_pprint_drast:bool (** whether to print data-race type-checked AST *)
  -> ?should_pprint_fir:bool (** whether to print serialisable frontend IR *)
  -> ?ignore_data_races:bool (** whether to ignore data races (execute program anyways) *)
  -> ?compile_out_file:string (** output serialised protobuf to a file *)
  -> Lexing.lexbuf (* Lex buffer to read the program from *)
  -> unit
