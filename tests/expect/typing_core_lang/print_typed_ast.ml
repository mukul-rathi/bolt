open Run_program

let print_typed_ast input_str =
  run_program (Lexing.from_string input_str) ~should_pprint_tast:true ()
