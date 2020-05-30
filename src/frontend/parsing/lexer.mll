(*****************************************************************************)
(*  This is the specification for the lexer                                  *)
(*****************************************************************************)

{
  (* Inside these curly braces we define helper functions that are    
     exposed to our OCaml source code
   *)

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
let generic_type_param =  ['A' -'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexer Rules
 * To disambiguate prefixes, Ocamllex applies:
 *   1) Longest match
 *   2) Match first rule (hence id is listed after keywords) 
 *)

rule read_token = parse 
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
  | "<" { LANGLE }
  | ">" { RANGLE }
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
  | "extends" {EXTENDS}
  | generic_type_param {GENERIC_TYPE}
  | "capability" { CAPABILITY }
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
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf } 
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
    | '"'      { read_string (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf } 
  | eof { EOF }
  | _ { read_single_line_comment lexbuf } 
  
and read_multi_line_comment = parse
  | "*/" { read_token lexbuf } 
  | newline { next_line lexbuf; read_multi_line_comment lexbuf } 
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf } 
  
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
