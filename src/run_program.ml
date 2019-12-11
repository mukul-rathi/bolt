open Core
open Parsing.Lex_and_parse
open Typing.Type_checker
open Interpreter

let maybe_pprint_ast should_pprint_ast pprintfun ast =
  if should_pprint_ast then (
    pprintfun Fmt.stdout ast ;
    Error (Error.of_string "")
    (* This ends the program (preserving existing regression tests if subsequent pipeline
       changes) *) )
  else Ok ast

(* End program if checking data races to preserve regression tests *)
let maybe_stop_program check_data_races typed_ast =
  if check_data_races then Error (Error.of_string "") else Ok typed_ast

let run_program lexbuf ~should_pprint_past ~should_pprint_tast ~check_data_races
    ~print_execution () =
  let open Result in
  parse_program lexbuf
  >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
  >>= type_check_program ~check_data_races
  >>= maybe_stop_program check_data_races
  >>= maybe_pprint_ast should_pprint_tast pprint_typed_ast
  >>= run_program ~print_execution:(if print_execution then Some Fmt.stdout else None)
  |> function
  | Ok value -> print_result Fmt.stdout value
  | Error e  -> eprintf "%s" (Error.to_string_hum e)
