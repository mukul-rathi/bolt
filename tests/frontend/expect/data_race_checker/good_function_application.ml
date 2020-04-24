open Core
open Print_data_race_checker_ast

let%expect_test "Function application" =
  print_data_race_checker_ast
    " 
    function int f (int x ){ x}
    void main(){
      f(4)
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
          └──Expr: Int:4 |}]

let%expect_test "Function application with multiple args " =
  print_data_race_checker_ast
    " 
    function int f (int x, int y){ x}
    void main(){
       f (3, 4)
   }
  " ;
  [%expect
    {|
    Program
    └── Function: _fii
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Param: y
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _fii
          └──Expr: Int:3
          └──Expr: Int:4 |}]

let%expect_test "Function application with no args " =
  print_data_race_checker_ast
    " 
    function int f ( ){ 4}
    void main(){
       f()
   }
  " ;
  [%expect
    {|
    Program
    └── Function: _f
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Type expr: Int
          └──Expr: Int:4
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _f
          └──() |}]
