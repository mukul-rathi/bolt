open Run_program

let print_data_race input_str =
  run_program (Lexing.from_string input_str) ~check_data_races:true ()
