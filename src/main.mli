(** This is the entry point for execution of Bolt programs.

    Execution options are as follows:

    main.exe [FILENAME]

    A list of execution options

    === flags ===

    [-check-data-races] Check programs for potential data-races

    [-print-execution] Print each step of the interpreter's execution

    [-print-parsed-ast] Pretty print the parsed AST of the program

    [-print-typed-ast] Pretty print the typed AST of the program

    [-build-info] print info about this build and exit

    [-version] print the version of this build and exit

    [-help] print this help text and exit (alias: -?) *)

val run_program :
     Lexing.lexbuf
  -> should_pprint_past:bool
  -> should_pprint_tast:bool
  -> check_data_races:bool
  -> print_execution:bool
  -> unit
  -> unit
