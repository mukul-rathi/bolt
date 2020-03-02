open Core
open Print_parsed_ast

let%expect_test "Constructor with multiple args" =
  print_parsed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar;
      const int h : Bar;
    }
    void main(){
      let x = new Foo(f:4, g:5, h:6);
        x
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: g
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: h
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:4
             └── Field: g
                └──Expr: Int:5
             └── Field: h
                └──Expr: Int:6
       └──Expr: Variable: x |}]
