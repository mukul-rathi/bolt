open Core
open Print_typed_ast

let%expect_test "Function return type matches" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function int f (int z) {
      z // good - returns an int 
    }
    void main() {
      f(1) 
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
      └── Function: f
         └── Return type: Int
         └──Param: z
            └──Type expr: Int
         └──Body block
            └──Type expr: Int
            └──Expr: Variable: z
               └──Type expr: Int
      └──Main block
         └──Type expr: Int
         └──Expr: Function App
            └──Type expr: Int
            └──Function: f
            └──Expr: Int:1 |}]

let%expect_test "Function void return type matches any type " =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function void f (int z) {
      z // good - throws away int return type 
    }
    void main() {
      f(1) 
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
      └── Function: f
         └── Return type: Void
         └──Param: z
            └──Type expr: Int
         └──Body block
            └──Type expr: Int
            └──Expr: Variable: z
               └──Type expr: Int
      └──Main block
         └──Type expr: Void
         └──Expr: Function App
            └──Type expr: Void
            └──Function: f
            └──Expr: Int:1 |}]
