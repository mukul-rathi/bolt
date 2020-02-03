open Core
open Print_data_race_checker_ast

let%expect_test "Arithmetic operators" =
  print_data_race_checker_ast
    " 
   void main(){
    (((5 + (5 % 2)) - 10) * (1 / 2))
   }
  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Int
       └──Expr: Bin Op: *
          └──Type expr: Int
          └──Expr: Bin Op: -
             └──Type expr: Int
             └──Expr: Bin Op: +
                └──Type expr: Int
                └──Expr: Int:5
                └──Expr: Bin Op: %
                   └──Type expr: Int
                   └──Expr: Int:5
                   └──Expr: Int:2
             └──Expr: Int:10
          └──Expr: Bin Op: /
             └──Type expr: Int
             └──Expr: Int:1
             └──Expr: Int:2 |}]

let%expect_test "Comparison operators" =
  print_data_race_checker_ast
    " 
       void main(){
      (4 < 5);
      let x = 4;
      (x <= x);
      (x > 3);
      (x >= 4);
      (x != 23);
      (x == 4)
    }
  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Bool
       └──Expr: Bin Op: <
          └──Type expr: Bool
          └──Expr: Int:4
          └──Expr: Int:5
       └──Expr: Let var: _var_x0
          └──Type expr: Int
          └──Expr: Int:4
       └──Expr: Bin Op: <=
          └──Type expr: Bool
          └──Expr: Variable: _var_x0
             └──Type expr: Int
          └──Expr: Variable: _var_x0
             └──Type expr: Int
       └──Expr: Bin Op: >
          └──Type expr: Bool
          └──Expr: Variable: _var_x0
             └──Type expr: Int
          └──Expr: Int:3
       └──Expr: Bin Op: >=
          └──Type expr: Bool
          └──Expr: Variable: _var_x0
             └──Type expr: Int
          └──Expr: Int:4
       └──Expr: Bin Op: !=
          └──Type expr: Bool
          └──Expr: Variable: _var_x0
             └──Type expr: Int
          └──Expr: Int:23
       └──Expr: Bin Op: ==
          └──Type expr: Bool
          └──Expr: Variable: _var_x0
             └──Type expr: Int
          └──Expr: Int:4 |}]

let%expect_test "Boolean operators" =
  print_data_race_checker_ast
    "
    void main(){
      (true || false) && (!false)
   }
  " ;
  [%expect
    {|
    Program
    └──Main block
       └──Type expr: Bool
       └──Expr: Bin Op: &&
          └──Type expr: Bool
          └──Expr: Bin Op: ||
             └──Type expr: Bool
             └──Expr: Bool:true
             └──Expr: Bool:false
          └──Expr: Unary Op: !
             └──Type expr: Bool
             └──Expr: Bool:false |}]
