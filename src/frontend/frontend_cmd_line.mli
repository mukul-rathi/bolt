(** This is the entry point for command-line frontend IR compilation of Bolt programs.

    Execution options are as follows:

    main.exe [FILENAME]

    A list of execution options

    === flags ===

    [-check-data-races] Check programs for potential data-races

    [-print-parsed-ast] Pretty print the parsed AST of the program

    [-print-typed-ast] Pretty print the typed AST of the program

    [-print-desugared-ast] Pretty print the desugared AST of the program

    [-print-ir-gen] Pretty print the generated IR of the program

    [-build-info] print info about this build and exit

    [-version] print the version of this build and exit

    [-help] print this help text and exit (alias: -?)

    =======

    If no flags are provided, then the serialised IR is output to stdout *)
