open Run_program

let print_parsed_ast input_str =
  run_program
    (Lexing.from_string input_str)
    ~should_pprint_past:true ~should_pprint_tast:false ~check_data_races:false
    ~print_execution:false ()
