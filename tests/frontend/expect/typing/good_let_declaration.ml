open Core
open Print_typed_ast

let%expect_test "Correct type annotation" =
  print_typed_ast " 
    void main(){ 
    let x : int = 1
    }
  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Int
          └──Expr: Int:1 |}]
