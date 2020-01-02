open Run_program

let print_desugared_ast input_str =
  run_program (Lexing.from_string input_str) ~should_pprint_dast:true
