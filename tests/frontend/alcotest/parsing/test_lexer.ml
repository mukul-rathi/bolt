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
      ignore (read_token (Lexing.from_string "/* ") : token))

let test_lex_token (input_str, token) =
  Alcotest.(check parser_token_testable)
    "same token" token
    (read_token (Lexing.from_string input_str))

let test_lex_tokens () =
  List.iter ~f:test_lex_token
    [ ("(", LPAREN); (")", RPAREN); ("{", LBRACE); ("}", RBRACE); (",", COMMA); (".", DOT)
    ; (":", COLON); (";", SEMICOLON); ("=", EQUAL); ("+", PLUS); ("-", MINUS); ("*", MULT)
    ; ("/", DIV); ("%", REM); ("<", LESS_THAN); (">", GREATER_THAN); ("&&", AND)
    ; ("||", OR); ("!", EXCLAMATION_MARK); ("let", LET); ("new", NEW); ("const", CONST)
    ; ("var", VAR); ("function", FUNCTION); ("consume", CONSUME); ("finish", FINISH)
    ; ("async", ASYNC); ("class", CLASS); ("capability", CAPABILITY); ("linear", LINEAR)
    ; ("local", LOCAL); ("read", READ); ("subordinate", SUBORDINATE); ("locked", LOCKED)
    ; ("int", TYPE_INT); ("bool", TYPE_BOOL); ("void", TYPE_VOID); ("borrowed", BORROWED)
    ; ("true", TRUE); ("false", FALSE); ("while", WHILE); ("if", IF); ("else", ELSE)
    ; ("for", FOR); ("main", MAIN); ("printf", PRINTF) ]

let test_lex_int =
  QCheck.Test.make ~count:100 ~name:"Lex integers"
    QCheck.(int)
    (fun i -> INT i = read_token (Lexing.from_string (Int.to_string i)))

let test_lex_string () = test_lex_token ("\"something here\"", STRING "something here")
let test_lex_string_newline () = test_lex_token ("\"\n\"", STRING "\n")

let test_lex_formatted_string () =
  test_lex_token ("\"Something %d is here\n\"", STRING "Something %d is here\n")

let test_lex_whitespace () = test_lex_token ("\" \"", STRING " ")
let test_lex_eof () = test_lex_token ("", EOF)
let test_lex_newline () = test_lex_token ("\n", EOF)
let test_lex_single_line_comment () = test_lex_token ("// Some comment", EOF)

let test_lex_multiline_comments () =
  test_lex_token ("/* Some \n multi-line comment */", EOF)

let () =
  let qcheck_lex = List.map ~f:QCheck_alcotest.to_alcotest [test_lex_int] in
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
        ; test_case "Lex single line comments" `Quick test_lex_single_line_comment
        ; test_case "Lex multi-line comments" `Quick test_lex_multiline_comments
        ; test_case "Lex string" `Quick test_lex_string
        ; test_case "Lex string newline" `Quick test_lex_string_newline
        ; test_case "Lex formatted string" `Quick test_lex_formatted_string ]
        @ qcheck_lex ) ]
