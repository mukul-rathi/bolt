open Core
open Print_data_race_checker_ast

let%expect_test "Constructor with multiple args" =
  print_data_race_checker_ast
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
       └──Type expr: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Variable: _var_x0
          └──Type expr: Foo
          └── Possible Capabilities:
             └── Possible Capability: Linear Bar |}]
