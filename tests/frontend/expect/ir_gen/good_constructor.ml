open Core
open Print_frontend_ir

let%expect_test "Constructor with multiple args" =
  print_frontend_ir
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
       └──VTable []
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _x0
          └──Expr: Constructor for: Foo
             └── Field: 0
                └──Expr: Int:4
             └── Field: 1
                └──Expr: Int:5
             └── Field: 2
                └──Expr: Int:6
       └──Expr: Variable: _x0 |}]
