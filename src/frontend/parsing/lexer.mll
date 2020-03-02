(*****************************************************************************)
(*  This is the specification for the lexer                                  *)
(*****************************************************************************)

{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexer Rules
 * To disambiguate prefixes, Ocamllex applies:
 *   1) Longest match
 *   2) Match first rule (hence id is listed after keywords) 
 *)

rule read_token = 
  parse 
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "%" { REM }
  | "<" { LESS_THAN }
  | ">" { GREATER_THAN }
  | "&&" { AND }
  | "||" { OR }
  | "!" { EXCLAMATION_MARK }
  | "let" { LET }
  | "new" { NEW }
  | "const" {CONST }
  | "var" { VAR }
  | "function" { FUNCTION }
  | "consume" { CONSUME }
  | "finish" { FINISH }
  | "async" { ASYNC }
  | "class" { CLASS }
  | "region" { REGION }
  | "linear" { LINEAR }
  | "local" { LOCAL }
  | "read" { READ }
  | "subordinate" { SUBORDINATE }
  | "locked" { LOCKED }
  | "int" { TYPE_INT }
  | "bool" { TYPE_BOOL } 
  | "void" { TYPE_VOID }
  | "borrowed" { BORROWED }
  | "true" { TRUE }
  | "false" { FALSE }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "main" { MAIN }
  | "printf" {PRINTF } 
  | whitespace { read_token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf } 
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
    | '"'      { read_string (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf } 
  | eof { EOF }
  | _ { single_line_comment lexbuf } 
  
and multi_line_comment = parse
  | "*/" { read_token lexbuf } 
  | newline { next_line lexbuf; multi_line_comment lexbuf } 
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { multi_line_comment lexbuf } 
  
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | [^ '"' ]+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
