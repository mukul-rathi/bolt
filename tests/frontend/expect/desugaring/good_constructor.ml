open Core
open Print_desugared_ast

let%expect_test "Constructor with multiple args" =
  print_desugared_ast
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
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
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
          └──Type expr: Class: Foo |}]
