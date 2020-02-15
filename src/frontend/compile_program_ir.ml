open Core
open Parsing.Lex_and_parse
open Typing.Type_program
open Desugaring.Desugar_program
open Data_race_checker.Type_data_races_program
open Ir_gen.Ir_gen_program
open Ir_gen.Ir_gen_protobuf

let maybe_pprint_ast should_pprint_ast pprintfun ast =
  if should_pprint_ast then (
    pprintfun Fmt.stdout ast ;
    Error (Error.of_string "")
    (* This ends the program (preserving existing regression tests if subsequent pipeline
       changes) *) )
  else Ok ast

let compile_program_ir ?(should_pprint_past = false) ?(should_pprint_tast = false)
    ?(should_pprint_dast = false) ?(should_pprint_fir = false) ?compile_out_file lexbuf =
  let open Result in
  parse_program lexbuf
  >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
  >>= type_program
  >>= maybe_pprint_ast should_pprint_tast pprint_typed_ast
  >>= desugar_program >>= type_data_races_program
  >>= maybe_pprint_ast should_pprint_dast pprint_data_race_checker_ast
  >>= ir_gen_program
  >>= maybe_pprint_ast should_pprint_fir pprint_frontend_ir
  |> function
  | Ok program -> (
    match compile_out_file with
    | Some file_name ->
        Out_channel.with_file file_name ~f:(fun file_oc ->
            ir_gen_protobuf program file_oc)
    | None           -> ir_gen_protobuf program stdout )
  | Error e    -> eprintf "%s" (Error.to_string_hum e)
