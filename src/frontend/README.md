## Frontend

This contains the code for the compiler frontend, written in OCaml.

The pipeline is as follows:

parsing -> typing -> desugaring -> data_race_checker -> ir_gen

This is specified in `compile_program_ir.ml`, which takes in a buffer to read the program from, and writes the serialised IR output by the frontend.

The `main.ml` file reads the Bolt file from the command line, along with other optional command-line arguments.
