open Core
open Parsing.Lex_and_parse
open Typing.Type_checker
open Interpreter.Execute_program

let maybe_pprint_ast should_pprint_ast pprintfun ast =
  if should_pprint_ast then (
    pprintfun Fmt.stdout ast ;
    Error (Error.of_string "")
    (* This ends the program (preserving existing regression tests if subsequent pipeline
       changes) *) )
  else Ok ast

let run_program ?(should_pprint_past = false) ?(should_pprint_tast = false)
    ?(print_execution = false) lexbuf =
  let open Result in
  parse_program lexbuf
  >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
  >>= type_check_program
  >>= maybe_pprint_ast should_pprint_tast pprint_typed_ast
  >>= execute_program ~print_execution:(if print_execution then Some Fmt.stdout else None)
  |> function
  | Ok value -> print_result Fmt.stdout value
  | Error e  -> eprintf "%s" (Error.to_string_hum e)
