open Core
open Lexer
open Lexing
open Format

(** Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program (filename : string) =
  In_channel.with_file filename ~f:(fun file_ic ->
      let lexbuf = Lexing.from_channel file_ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      try Ok (Parser.program Lexer.read_token lexbuf) with
      (* Unfortunately the lexer and parser throw exceptions - so here we swallow the exn
         into the Result monad*)
      | SyntaxError msg ->
          let error_msg = sprintf "%s: %s@." (print_error_position lexbuf) msg in
          Error (Error.of_string error_msg)
      | Parser.Error ->
          let error_msg = sprintf "%s: syntax error@." (print_error_position lexbuf) in
          Error (Error.of_string error_msg))

let pprint_ast ppf (prog : Parsed_ast.program) = Pprint_past.pprint_program ppf prog
