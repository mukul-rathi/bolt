open Core
open Print_parsed_ast

let%expect_test "Good if statement" =
  print_parsed_ast
    " 
   {
     let x = true;
     if x {
       0
     }
     else {
       1
     }
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: Block
         └──Expr: Let var: x
            └──Expr: Bool:true
         └──Expr: If
            └──Expr: Variable: x
            └──Expr: Block
               └──Expr: Int:0
            └──Expr: Block
               └──Expr: Int:1 |}]

let%expect_test "Good if then statement" =
  print_parsed_ast " 
   {
     let x = true;
     if x then 0 else 1
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: Block
         └──Expr: Let var: x
            └──Expr: Bool:true
         └──Expr: If
            └──Expr: Variable: x
            └──Expr: Int:0
            └──Expr: Int:1 |}]

let%expect_test "Good while loop" =
  print_parsed_ast " 
   while (1 < 2){
     let x = 5
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: While
         └──Expr: Bin Op: <
            └──Expr: Int:1
            └──Expr: Int:2
         └──Expr: Block
            └──Expr: Let var: x
               └──Expr: Int:5 |}]

let%expect_test "Good for loop" =
  print_parsed_ast " 
   for i in range(0, (5*5), 1) {
     i
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: For: loop var: i
         └──Expr: Int:0
         └──Expr: Bin Op: *
            └──Expr: Int:5
            └──Expr: Int:5
         └──Expr: Int:1
         └──Expr: Block
            └──Expr: Variable: i |}]
