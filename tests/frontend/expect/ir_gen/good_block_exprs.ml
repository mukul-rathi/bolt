open Core
open Print_frontend_ir

let%expect_test "Block of exprs" =
  print_frontend_ir
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
       └──Param: Int x
       └──Body block
          └──Expr: Variable: x
             └──Locked false
    └──Main expr
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:4
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:5
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:6 |}]
