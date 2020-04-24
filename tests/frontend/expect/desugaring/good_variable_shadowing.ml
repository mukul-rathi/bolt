open Core
open Print_desugared_ast

let%expect_test "Variable nested shadowing" =
  print_desugared_ast
    "

   void main() {
    let x = 5; 
    while(x <4){
      printf(\"Value of x %d\", x); // this is outer x
      let x = 1;
      printf(\"Value of x %d\", x) // this is nested x
    }
  }

  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Void
       └──Expr: Let var: _var_x0
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: While
          └──Type expr: Void
          └──Expr: Bin Op: <
             └──Type expr: Bool
             └──Expr: Variable: _var_x0
                └──Type expr: Int
             └──Expr: Int:4
          └──Body block
             └──Type expr: Void
             └──Expr: Printf
                └──Value of x %d
                └──Expr: Variable: _var_x0
                   └──Type expr: Int
             └──Expr: Let var: _var_x1
                └──Type expr: Int
                └──Expr: Int:1
             └──Expr: Printf
                └──Value of x %d
                └──Expr: Variable: _var_x1
                   └──Type expr: Int |}]
