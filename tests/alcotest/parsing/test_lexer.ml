open Parsing
open Lexer
open Parser
open Pprint_parser_tokens
open Core

let parser_token_testable =
  Alcotest.testable pprint_tokens (fun token1 token2 -> token1 = token2)

let test_illegal_character () =
  Alcotest.check_raises "Syntax Error" (SyntaxError "Lexer - Illegal character: ?")
    (fun () -> ignore (read_token (Lexing.from_string "?") : token))

let test_unterminated_comment () =
  Alcotest.check_raises "Syntax Error"
    (SyntaxError "Lexer - Unexpected EOF - please terminate your comment.") (fun () ->
      ignore (read_token (Lexing.from_string "(* ") : token))

let test_lex_token (input_str, token) =
  Alcotest.(check parser_token_testable)
    "same token" token
    (read_token (Lexing.from_string input_str))

let test_lex_tokens () =
  List.iter ~f:test_lex_token
    [ ("(", LPAREN); (")", RPAREN); ("{", LBRACE); ("}", RBRACE); (",", COMMA); (".", DOT)
    ; (":", COLON); (";", SEMICOLON); ("=", EQUAL); (":=", ASSIGN); ("let", LET)
    ; ("new", NEW); ("const", CONST); ("var", VAR); ("function", FUNCTION)
    ; ("consume", CONSUME); ("finish", FINISH); ("async", ASYNC); ("class", CLASS)
    ; ("trait", TRAIT); ("require", REQUIRE); ("linear", LINEAR); ("thread", THREAD)
    ; ("read", READ); ("int", TYPE_INT); ("foo", ID "foo") ]

let test_lex_int =
  QCheck.Test.make ~count:100 ~name:"Lex integers"
    QCheck.(int)
    (fun i -> INT i = read_token (Lexing.from_string (Int.to_string i)))

let test_lex_whitespace () = test_lex_token (" ", EOF)
let test_lex_eof () = test_lex_token ("", EOF)
let test_lex_newline () = test_lex_token ("\n", EOF)
let test_lex_comments () = test_lex_token ("(* Some \n comment *)", EOF)

let () =
  let qcheck_lex_int = List.map ~f:QCheck_alcotest.to_alcotest [test_lex_int] in
  let open Alcotest in
  run "Lexer"
    [ ( "Syntax Errors"
      , [ test_case "Illegal characters" `Quick test_illegal_character
        ; test_case "Unterminated comments" `Quick test_unterminated_comment ] )
    ; ( "Accepted Tokens"
      , [ test_case "Lex tokens" `Quick test_lex_tokens
        ; test_case "Lex whitespace" `Quick test_lex_whitespace
        ; test_case "Lex eof" `Quick test_lex_eof
        ; test_case "Lex new line" `Quick test_lex_newline
        ; test_case "Lex comments" `Quick test_lex_comments ]
        @ qcheck_lex_int ) ]
