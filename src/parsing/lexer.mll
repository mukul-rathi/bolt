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
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | ":=" { ASSIGN }
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {MULT}
  | "/" {DIV}
  | "%" {REM}
  | "<" {LESS_THAN}
  | ">" {GREATER_THAN}
  | "&&" {AND}
  | "||" {OR}
  | "!" {EXCLAMATION_MARK}
  | "let" { LET }
  | "new" { NEW }
  | "const" {CONST }
  | "var" { VAR }
  | "function" { FUNCTION }
  | "consume" { CONSUME }
  | "finish" { FINISH }
  | "async" { ASYNC }
  | "class" { CLASS }
  | "trait" { TRAIT }
  | "require" { REQUIRE }
  | "linear" { LINEAR }
  | "thread" { THREAD }
  | "read" { READ }
  | "int" { TYPE_INT }
  | "bool" { TYPE_BOOL } 
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | whitespace { read_token lexbuf }
  | "(*" { comment lexbuf } 
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }


and comment = parse
  | "*)" { read_token lexbuf } 
  | newline { next_line lexbuf; comment lexbuf } 
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { comment lexbuf } 
