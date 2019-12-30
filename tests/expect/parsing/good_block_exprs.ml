open Core
open Print_parsed_ast

let%expect_test "Block of exprs" =
  print_parsed_ast
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
       └──Expr: Block
          └──Expr: Variable: x
    └──Expr: Block
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:4
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:5
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:6 |}]
