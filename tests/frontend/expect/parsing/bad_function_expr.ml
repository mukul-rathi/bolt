open Core
open Print_parsed_ast

let%expect_test "Function argument not annotated with type" =
  print_parsed_ast " 
    function int f (x){ x}
    void main(){
      f(4)
    }
  " ;
  [%expect {| Line:2 Position:23: syntax error |}]

let%expect_test "Function not annotated with return type" =
  print_parsed_ast " 
    function f (int x){ x}
    void main(){
      f(4)
    }
  " ;
  [%expect {| Line:2 Position:17: syntax error |}]
