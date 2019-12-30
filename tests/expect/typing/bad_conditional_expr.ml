open Core
open Print_typed_ast

let%expect_test "If statement with non-boolean condition" =
  print_typed_ast
    " 
   void main(){
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
   void main(){
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

let%expect_test "While loop with non-boolean condition" =
  print_typed_ast
    " 
   void main(){
     while 1 { (* 1 is not a boolean value *)
       0
     }; 4
    }
  " ;
  [%expect
    {|
      Line:3 Position:6 Type error - While loop condition expression should have boolean type but instead has type Int |}]

let%expect_test "For loop with non-boolean condition" =
  print_typed_ast
    " 
  void main(){
   for (let i = 0; i ; i := i+1 ) {
     i
   }
   }
  " ;
  [%expect
    {|
      Line:3 Position:4 Type error - For loop condition expression should have boolean type but instead has type Int |}]
