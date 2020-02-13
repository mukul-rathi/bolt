open Core
open Print_frontend_ir

let%expect_test "Immutable refs in multiple threads" =
  print_frontend_ir
    " 
    class Foo  {
       region read Bar;
      const int f : Bar;
    }
   function int test() {
         5
      }
   void main() {
      let x = new Foo(f:5);
      let y = 5;
      finish{
        (* can read aliases in different threads as neither are mutable *)
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
       └──Field: Thread ID
       └──Field: Lock Counter
       └──Field: Int
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Expr: Int:5
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: 2
                └──Expr: Int:5
       └──Expr: Let var: _var_y0
          └──Expr: Int:5
       └──Expr: Finish_async
             └── Async Expr Free Vars:
                └── (_var_x0, _var_y0)
             └──Async Expr block
                └──Expr: Variable: _var_x0
                   └──Locked false
                └──Expr: Function App
                   └──Function: test
                   └──()
                └──Expr: Variable: _var_y0
                   └──Locked false
          └──Current Thread Expr block
             └──Expr: Variable: _var_x0
                └──Locked false
             └──Expr: Variable: _var_y0
                └──Locked false
       └──Expr: Objfield: _var_x0[2]
          └──Locked false |}]
