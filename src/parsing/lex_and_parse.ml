open Core
open Lexer
open Lexing
open Format

(** Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf str_formatter "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1) ;
  flush_str_formatter ()

let parse_program (filename : string) =
  In_channel.with_file filename ~f:(fun file_ic ->
      let lexbuf = Lexing.from_channel file_ic in
      lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= filename} ;
      try Parser.program Lexer.read_token lexbuf with
      | SyntaxError msg ->
          fprintf err_formatter "%s: %s@." (print_error_position lexbuf) msg ;
          None
      | Parser.Error ->
          fprintf err_formatter "%s: syntax error@." (print_error_position lexbuf) ;
          None)

let pprint_ast ppf (prog : Ast_types.program) = Pprint_ast.pprint_program ppf prog
