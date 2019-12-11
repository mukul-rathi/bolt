open Run_program

let print_typed_ast input_str =
  run_program
    (Lexing.from_string input_str)
    ~should_pprint_past:false ~should_pprint_tast:true ~check_data_races:false
    ~print_execution:false ()
