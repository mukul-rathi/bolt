open Core
open Print_typed_ast

let%expect_test "Function return type matches" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    function int f (int z) {
      z (* good - returns an int *)
    }
    void main() {
      f(1) 
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Regions:
            └──Region: Linear Bar
         └──Field Defn: f
            └──Mode: Var
            └──Type expr: Int
            └──Regions: Bar
      └── Function: f
         └── Return type: Int
         └──Param: z
            └──Type expr: Int
         └──Expr: Block
            └──Type expr: Int
            └──Expr: Variable: z
               └──Type expr: Int
      └──Expr: Block
         └──Type expr: Int
         └──Expr: Function App
            └──Type expr: Int
            └──Function: f
            └──Expr: Int:1 |}]

let%expect_test "Function void return type matches any type " =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    function void f (int z) {
      z (* good - throws away int return type *)
    }
    void main() {
      f(1) 
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Regions:
            └──Region: Linear Bar
         └──Field Defn: f
            └──Mode: Var
            └──Type expr: Int
            └──Regions: Bar
      └── Function: f
         └── Return type: Void
         └──Param: z
            └──Type expr: Int
         └──Expr: Block
            └──Type expr: Int
            └──Expr: Variable: z
               └──Type expr: Int
      └──Expr: Block
         └──Type expr: Void
         └──Expr: Function App
            └──Type expr: Void
            └──Function: f
            └──Expr: Int:1 |}]
