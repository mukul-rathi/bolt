open Core
open Print_data_race_checker_ast

let%expect_test "Immutable refs in multiple threads" =
  print_data_race_checker_ast
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
       └──Body block
          └──Type expr: Int
          └──Expr: Int:5
    └──Main block
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
             └── Async Expr Free Vars:
                └── ((Foo) _var_x0)
             └──Async Expr block
                └──Type expr: Int
                └──Expr: Variable: _var_x0
                   └──Type expr: Class: Foo
                   └──Regions:
                      └──Region: Read Bar
                └──Expr: Function App
                   └──Type expr: Int
                   └──Function: test
                   └──()
                └──Expr: Variable: _var_y0
                   └──Type expr: Int
       └── Current Thread Expr Free Vars:
          └── (_var_x0)
          └──Current thread block
             └──Type expr: Int
             └──Expr: Variable: _var_x0
                └──Type expr: Class: Foo
                └──Regions:
                   └──Region: Read Bar
             └──Expr: Variable: _var_y0
                └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) _var_x0.f
          └──Type expr: Int
          └──Regions:
             └──Region: Read Bar |}]
