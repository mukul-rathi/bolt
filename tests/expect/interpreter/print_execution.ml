open Run_program

let print_execution input_str =
  run_program (Lexing.from_string input_str) ~print_execution:true
