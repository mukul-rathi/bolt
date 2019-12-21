open Core
open Print_typed_ast

let%expect_test "If statement with non-boolean condition" =
  print_typed_ast
    " 
   {
     if 1 { (* 1 is not a boolean value *)
       0
     }
     else {
       1
     }
   }
  " ;
  [%expect
    {|
      Line:3 Position:6 Type error - If statement condition expression should have boolean type but instead has type Int |}]

let%expect_test "If statement then and else branches' types are different " =
  print_typed_ast
    " 
   {
     if true { 
       0
     }
     else {
       false
     }
   }
  " ;
  [%expect
    {|
      Line:3 Position:6 Type error - If statement branches' types' not consistent - then branch has type Int but else branch has type Bool |}]
