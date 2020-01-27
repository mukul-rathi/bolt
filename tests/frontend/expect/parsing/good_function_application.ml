open Core
open Print_parsed_ast

let%expect_test "Function application" =
  print_parsed_ast
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
       └──Param: x
          └──Type expr: Int
       └──Expr: Block
          └──Expr: Variable: x
    └──Expr: Block
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:4 |}]

let%expect_test "Function application with multiple args " =
  print_parsed_ast
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
          └──Expr: Variable: x
    └──Expr: Block
       └──Expr: Function App
          └──Function: f
          └──Expr: Int:3
          └──Expr: Int:4 |}]

let%expect_test "Function application with no args " =
  print_parsed_ast " 
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
          └──Expr: Int:4
    └──Expr: Block
       └──Expr: Function App
          └──Function: f
          └──() |}]

let%expect_test "Function application with boolean arg" =
  print_parsed_ast
    " 
    function bool f (bool b ){ b }
    void main(){
       f(true);
       f(false)
    }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Bool
       └──Param: b
          └──Type expr: Bool
       └──Expr: Block
          └──Expr: Variable: b
    └──Expr: Block
       └──Expr: Function App
          └──Function: f
          └──Expr: Bool:true
       └──Expr: Function App
          └──Function: f
          └──Expr: Bool:false |}]
