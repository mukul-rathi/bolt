open Core
open Print_typed_ast

let%expect_test "Immutable refs in multiple locals" =
  print_typed_ast
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
       └──Capabilities:
          └──Capability: Read Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Type expr: Int
          └──Expr: Int:5
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
       └──Expr: Let var: y
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: Finish_async
          └──Type expr: Int
             └──Async Expr block
                └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Class: Foo
                └──Expr: Function App
                   └──Type expr: Int
                   └──Function: test
                   └──()
                └──Expr: Variable: y
                   └──Type expr: Int
          └──Current thread block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Class: Foo
             └──Expr: Variable: y
                └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) x.f
          └──Type expr: Int |}]
