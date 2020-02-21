open Core
open Print_parsed_ast

let%expect_test "Constructor with multiple args" =
  print_parsed_ast
    " 
    class Foo {
      region linear Bar;
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
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
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
