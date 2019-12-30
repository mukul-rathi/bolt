open Core
open Print_typed_ast

let%expect_test "Accessing a free variable" =
  print_typed_ast " 
  void main(){
    x 
  }
  " ;
  [%expect {|
    Line:3 Position:5 Type error - Variable not defined in environment |}]
