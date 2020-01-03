open Core
open Parsing.Lex_and_parse
open Typing.Type_program
open Desugaring.Desugar_program
open Ir_gen.Ir_gen_program

let maybe_pprint_ast should_pprint_ast pprintfun ast =
  if should_pprint_ast then (
    pprintfun Fmt.stdout ast ;
    Error (Error.of_string "")
    (* This ends the program (preserving existing regression tests if subsequent pipeline
       changes) *) )
  else Ok ast

let run_program ?(should_pprint_past = false) ?(should_pprint_tast = false)
    ?(should_pprint_dast = false) ?(should_pprint_last = false) lexbuf =
  let open Result in
  parse_program lexbuf
  >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
  >>= type_program
  >>= maybe_pprint_ast should_pprint_tast pprint_typed_ast
  >>= desugar_program
  >>= maybe_pprint_ast should_pprint_dast pprint_desugared_ast
  >>= ir_gen_program
  >>= maybe_pprint_ast should_pprint_last pprint_llvm_ast
  |> function Ok _ -> () | Error e -> eprintf "%s" (Error.to_string_hum e)
