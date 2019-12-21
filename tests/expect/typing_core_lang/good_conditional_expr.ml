open Core
open Print_typed_ast

let%expect_test "Good if statement" =
  print_typed_ast
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
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Bool
            └──Expr: Bool:true
         └──Expr: Finish_async
            └──Type expr: Int
            └──Expr: Variable: x
               └──Type expr: Bool
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Int:0
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Int:1 |}]
