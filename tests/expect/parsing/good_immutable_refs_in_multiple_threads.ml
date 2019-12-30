open Core
open Print_parsed_ast

let%expect_test "Immutable refs in multiple threads" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:5
       └──Expr: Let var: y
          └──Expr: Int:5
       └──Expr: Finish async
          └── Async Expr:
             └──Expr: Block
                └──Expr: Variable: x
                └──Expr: Variable: y
          └──Expr: Block
             └──Expr: Variable: x
             └──Expr: Variable: y
       └──Expr: Objfield: x.f |}]
