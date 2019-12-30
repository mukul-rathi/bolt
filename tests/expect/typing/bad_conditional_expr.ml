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

let%expect_test "While loop with non-boolean condition" =
  print_typed_ast
    " 
   {
     while 1 { (* 1 is not a boolean value *)
       0
     }; 4
    }
  " ;
  [%expect
    {|
      Line:3 Position:6 Type error - While loop condition expression should have boolean type but instead has type Int |}]

let%expect_test "Bad for loop - start val is not an int" =
  print_typed_ast " 
   for i in range(true, (5*5), 1) {
     i
   }
  " ;
  [%expect
    {|
      Line:2 Position:4 Type error - For loop range expression - start should have type Int but instead has type Bool |}]

let%expect_test "Bad for loop - end val is not an int" =
  print_typed_ast " 
   for i in range(0, (5 >= 1), 1) {
     i
   }
  " ;
  [%expect
    {|
      Line:2 Position:4 Type error - For loop range expression - end should have type Int but instead has type Bool |}]

let%expect_test "Bad for loop - step val is not an int" =
  print_typed_ast " 
   for i in range(0, (5*5), true) {
     i
   }
  " ;
  [%expect
    {|
      Line:2 Position:4 Type error - For loop range expression - step should have type Int but instead has type Bool |}]
