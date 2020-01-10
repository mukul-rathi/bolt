open Core
open Print_typed_ast

let%expect_test "Function application" =
  print_typed_ast " 
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
       └──Param: x
          └──Type expr: Int
       └──Expr: Block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:4 |}]

let%expect_test "Function application with multiple args " =
  print_typed_ast
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
       └──Param: x
          └──Type expr: Int
       └──Param: y
          └──Type expr: Int
       └──Expr: Block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:3
          └──Expr: Int:4 |}]

let%expect_test "Function application with no args " =
  print_typed_ast " 
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
       └──Expr: Block
          └──Type expr: Int
          └──Expr: Int:4
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──() |}]
