open Compile_program_ir

let print_desugared_ast input_str =
  compile_program_ir (Lexing.from_string input_str) ~should_pprint_dast:true
