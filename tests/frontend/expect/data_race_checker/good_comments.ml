open Core
open Print_data_race_checker_ast

let%expect_test "Comments interspersed with code" =
  print_data_race_checker_ast
    " 
    void main(){
    /* This is a comment - it should not be parsed */ 
    let x = 4;//  Can occur after a line 
    let y /*Or even midway*/ = 5;
    /* Or before */ x
    /*
    Comments
    Can 
    Span 
    Multiple 
    Lines
    */
    }
  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _x0
          └──Type expr: Int
          └──Expr: Int:4
       └──Expr: Let var: _y0
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: Variable: _x0
          └──Type expr: Int |}]
