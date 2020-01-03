open Core
open Print_parsed_ast

let%expect_test "Arithmetic operators" =
  print_parsed_ast " 
  void main(){
    (((5 + (5 % 2)) - 10) * (1 / 2))
   }
  " ;
  [%expect
    {|
    Program
    └──Expr: Block
       └──Expr: Bin Op: *
          └──Expr: Bin Op: -
             └──Expr: Bin Op: +
                └──Expr: Int:5
                └──Expr: Bin Op: %
                   └──Expr: Int:5
                   └──Expr: Int:2
             └──Expr: Int:10
          └──Expr: Bin Op: /
             └──Expr: Int:1
             └──Expr: Int:2 |}]

let%expect_test "Comparison operators" =
  print_parsed_ast
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
    └──Expr: Block
       └──Expr: Bin Op: <
          └──Expr: Int:4
          └──Expr: Int:5
       └──Expr: Let var: x
          └──Expr: Int:4
       └──Expr: Bin Op: <=
          └──Expr: Variable: x
          └──Expr: Variable: x
       └──Expr: Bin Op: >
          └──Expr: Variable: x
          └──Expr: Int:3
       └──Expr: Bin Op: >=
          └──Expr: Variable: x
          └──Expr: Int:4
       └──Expr: Bin Op: !=
          └──Expr: Variable: x
          └──Expr: Int:23
       └──Expr: Bin Op: ==
          └──Expr: Variable: x
          └──Expr: Int:4 |}]

let%expect_test "Boolean operators" =
  print_parsed_ast "
    void main(){
      (true || false) && (!false)
   }
  " ;
  [%expect
    {|
    Program
    └──Expr: Block
       └──Expr: Bin Op: &&
          └──Expr: Bin Op: ||
             └──Expr: Bool:true
             └──Expr: Bool:false
          └──Expr: Unary Op: !
             └──Expr: Bool:false |}]
