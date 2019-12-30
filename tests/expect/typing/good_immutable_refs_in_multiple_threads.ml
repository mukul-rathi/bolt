open Core
open Print_typed_ast

let%expect_test "Immutable refs in multiple threads" =
  print_typed_ast
    " 
    class Foo  {
       region read Bar;
      const int f : Bar;
    }
   void main() {
      let x = new Foo(f:5);
      let y = 5;
      finish{
        (* can read aliases in different threads as neither are mutable *)
        async {
          x;
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
    └──Expr: Block
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
          └── Async Expr:
             └──Expr: Block
                └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Class: Foo
                └──Expr: Variable: y
                   └──Type expr: Int
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Class: Foo
             └──Expr: Variable: y
                └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) x.f
          └──Type expr: Int |}]
