open Core
open Print_frontend_ir

let%expect_test "Function application" =
  print_frontend_ir
    " 
    function int f (int x ){ x}
    void main(){
      f(4)
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
    └──Main expr
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:4 |}]

let%expect_test "Function application with multiple args " =
  print_frontend_ir
    " 
    function int f (int x, int y){ x}
    void main(){
       f (3, 4)
   }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: Int x
       └──Param: Int y
       └──Body block
          └──Expr: Variable: x
    └──Main expr
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:3
          └──Expr: Int:4 |}]

let%expect_test "Function application with no args " =
  print_frontend_ir " 
    function int f ( ){ 4}
    void main(){
       f()
   }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Expr: Int:4
    └──Main expr
       └──Expr: Function App
          └──Function: f
          └──Expr: () |}]
