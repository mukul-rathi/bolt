open Core
open Print_frontend_ir

let%expect_test "Immutable refs in multiple locals" =
  print_frontend_ir
    " 
    class Foo  {
       capability read Bar;
      const int f : Bar;
    }
   function int test() {
         5
      }
   void main() {
      let x = new Foo(f:5);
      let y = 5;
      finish{
        // can read aliases in different locals as neither are mutable 
        async {
           x;
          test();
          y
        }
          x;
          y
      };
      x.f
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──VTable []
       └──Field: Int
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Expr: Int:5
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: 0
                └──Expr: Int:5
       └──Expr: Let var: _var_y0
          └──Expr: Int:5
       └──Expr: Finish_async
             └── Async Expr Free Vars:
                └── (_var_x0)
             └──Async Expr block
                └──Expr: Variable: _var_x0
                └──Expr: Function App
                   └──Function: test
                   └──()
                └──Expr: Variable: _var_y0
          └──Current ThreadLocal Expr block
             └──Expr: Variable: _var_x0
             └──Expr: Variable: _var_y0
       └──Expr: Objfield: _var_x0[0] |}]
