open Core
open Print_parsed_ast

let%expect_test "Function argument not annotated with type" =
  print_parsed_ast " 
    (fun x -> x end) 4
  " ;
  [%expect {| Line:2 Position:14: syntax error |}]
