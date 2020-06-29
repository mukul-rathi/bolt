open Core
open Print_frontend_ir

let%expect_test "Arithmetic operators" =
  print_frontend_ir " 
   void main(){
    (((5 + (5 % 2)) - 10) * (1 / 2))
   }
  " ;
  [%expect
    {|
    Program
    └──Main expr
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
  print_frontend_ir
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
    └──Main expr
       └──Expr: Bin Op: <
          └──Expr: Int:4
          └──Expr: Int:5
       └──Expr: Let var: _x0
          └──Expr: Int:4
       └──Expr: Bin Op: <=
          └──Expr: Variable: _x0
          └──Expr: Variable: _x0
       └──Expr: Bin Op: >
          └──Expr: Variable: _x0
          └──Expr: Int:3
       └──Expr: Bin Op: >=
          └──Expr: Variable: _x0
          └──Expr: Int:4
       └──Expr: Bin Op: !=
          └──Expr: Variable: _x0
          └──Expr: Int:23
       └──Expr: Bin Op: ==
          └──Expr: Variable: _x0
          └──Expr: Int:4 |}]

let%expect_test "Boolean operators" =
  print_frontend_ir "
    void main(){
      (true || false) && (!false)
   }
  " ;
  [%expect
    {|
    Program
    └──Main expr
       └──Expr: Bin Op: &&
          └──Expr: Bin Op: ||
             └──Expr: Bool:true
             └──Expr: Bool:false
          └──Expr: Unary Op: !
             └──Expr: Bool:false |}]
