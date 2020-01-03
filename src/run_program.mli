(** This is the entry point for execution of Bolt programs. *)

val run_program :
     ?should_pprint_past:bool (** whether to print parsed AST *)
  -> ?should_pprint_tast:bool (** whether to print typed AST *)
  -> ?should_pprint_dast:bool (** whether to print desugared AST *)
  -> ?should_pprint_last:bool (** whether to print serialisable IR *)
  -> ?optional_filename:string (** output serialised protobuf to a file *)
  -> Lexing.lexbuf (* Lex buffer to read the program from *)
  -> unit
