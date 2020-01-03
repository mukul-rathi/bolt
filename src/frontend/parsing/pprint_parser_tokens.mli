(** This module is responsible for pretty-printing the tokens output by the lexer. It is
    used in the Alcotest lexer unit tests*)

val pprint_tokens : Parser.token Fmt.t
