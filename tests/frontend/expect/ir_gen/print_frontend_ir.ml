open Compile_program_ir

let print_frontend_ir input_str =
  compile_program_ir (Lexing.from_string input_str) ~should_pprint_fir:true
