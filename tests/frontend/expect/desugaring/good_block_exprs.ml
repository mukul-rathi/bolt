open Core
open Print_desugared_ast

let%expect_test "Block of exprs" =
  print_desugared_ast
    " 
    function int f (int x){x}
    void main(){ 
    f(4);
    f(5);
    f(6)
    }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main expr
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:4
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:5
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:6 |}]
