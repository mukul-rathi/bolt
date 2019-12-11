open Core
open Print_typed_ast

let%expect_test "Accessing a free variable" =
  print_typed_ast " 
    x 
  " ;
  [%expect {|
    Line:2 Position:5 Type error - Variable not defined in environment |}]
