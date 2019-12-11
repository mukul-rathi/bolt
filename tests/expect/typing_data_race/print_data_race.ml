open Run_program

let print_data_race input_str =
  run_program
    (Lexing.from_string input_str)
    ~should_pprint_past:false ~should_pprint_tast:false ~check_data_races:true
    ~print_execution:false ()
