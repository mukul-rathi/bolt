open Core
open Parsing.Lex_and_parse
open Typing.Type_checker
open Interpreter

let get_file_extension filename =
  String.split_on_chars filename ~on:['.'] |> List.last |> Option.value ~default:""

let bolt_file =
  let error_not_file filename =
    eprintf "'%s' is not a bolt file. Hint: use the .bolt extension\n%!" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes ->
          if get_file_extension filename = "bolt" then filename
          else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

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

let run_program filename should_pprint_past should_pprint_tast check_data_races
    print_execution () =
  let open Result in
  In_channel.with_file filename ~f:(fun file_ic ->
      let lexbuf = Lexing.from_channel file_ic in
      parse_program lexbuf)
  >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
  >>= type_check_program ~check_data_races
  >>= maybe_stop_program check_data_races
  >>= maybe_pprint_ast should_pprint_tast pprint_typed_ast
  >>= run_program ~print_execution:(if print_execution then Some Fmt.stdout else None)
  |> function
  | Ok value -> print_result Fmt.stdout value
  | Error e  -> eprintf "%s" (Error.to_string_hum e)

let command =
  Command.basic ~summary:"Run bolt programs"
    ~readme:(fun () -> "A list of execution options")
    Command.Let_syntax.(
      let%map_open should_pprint_past =
        flag "-print-parsed-ast" no_arg
          ~doc:" Pretty print the parsed AST of the program"
      and should_pprint_tast =
        flag "-print-typed-ast" no_arg ~doc:" Pretty print the typed AST of the program"
      and check_data_races =
        flag "-check-data-races" no_arg ~doc:"Check programs for potential data-races"
      and print_execution =
        flag "-print-execution" no_arg
          ~doc:"Print each step of the interpreter's execution"
      and filename = anon (maybe_with_default "-" ("filename" %: bolt_file)) in
      run_program filename should_pprint_past should_pprint_tast check_data_races
        print_execution)

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
