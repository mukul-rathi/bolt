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
