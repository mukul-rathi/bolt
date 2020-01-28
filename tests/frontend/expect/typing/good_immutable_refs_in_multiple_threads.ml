open Core
open Print_typed_ast

let%expect_test "Immutable refs in multiple threads" =
  print_typed_ast
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
       └──Regions:
          └──Region: Read Bar
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Expr: Block
          └──Type expr: Int
          └──Expr: Int:5
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
       └──Expr: Let var: _var_y0
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: Finish_async
          └──Type expr: Int
          └── Async Expr:
             └──Expr: Block
                └──Type expr: Int
                └──Expr: Variable: _var_x0
                   └──Type expr: Class: Foo
                └──Expr: Function App
                   └──Type expr: Int
                   └──Function: test
                   └──()
                └──Expr: Variable: _var_y0
                   └──Type expr: Int
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Variable: _var_x0
                └──Type expr: Class: Foo
             └──Expr: Variable: _var_y0
                └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) _var_x0.f
          └──Type expr: Int |}]
