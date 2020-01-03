open Core
open Print_llvm_ast

let%expect_test "Immutable refs in multiple threads" =
  print_llvm_ast
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
       └──Field: Int f
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Expr: Int:5
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:5
       └──Expr: Let var: _var_y0
          └──Expr: Int:5
       └──Expr: Finish_async
             └──Async Expr block
                └──Expr: Variable: _var_x0
                └──Expr: Function App
                   └──Function: test
                   └──Expr: ()
                └──Expr: Variable: _var_y0
          └──Current Thread Expr block
             └──Expr: Variable: _var_x0
             └──Expr: Variable: _var_y0
       └──Expr: Objfield: _var_x0.f |}]
