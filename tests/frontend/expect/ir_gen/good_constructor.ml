open Core
open Print_frontend_ir

let%expect_test "Constructor with multiple args" =
  print_frontend_ir
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
       └──Field: Int f
       └──Field: Int g
       └──Field: Int h
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:4
             └── Field: g
                └──Expr: Int:5
             └── Field: h
                └──Expr: Int:6
       └──Expr: Variable: _var_x0 |}]
