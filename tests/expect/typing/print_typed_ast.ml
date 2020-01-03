open Compile_program_ir

let print_typed_ast input_str =
  compile_program_ir (Lexing.from_string input_str) ~should_pprint_tast:true
