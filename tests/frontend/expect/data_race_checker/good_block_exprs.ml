open Core
open Print_data_race_checker_ast

let%expect_test "Block of exprs" =
  print_data_race_checker_ast
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
    └── Function: _fi
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _fi
          └──Expr: Int:4
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _fi
          └──Expr: Int:5
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _fi
          └──Expr: Int:6 |}]
