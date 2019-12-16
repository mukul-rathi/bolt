open Core
open Print_parsed_ast

let%expect_test "Immutable refs in multiple threads" =
  print_parsed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = new Foo(f:5) in 
      let y = 5 in 
      finish{
        (* can read aliases in different threads as neither are mutable *)
        async {
          x;
          y
        }
        async{
          x;
          y
        }
      } ;
      x.f
      end
    end
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Read
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
    └──Trait: Bar
       └──Cap: Read
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
    └──Expr: Let var: x
       └──Expr: Constructor for: Foo
          └── Field: f
             └──Expr: Int:5
       └──Expr: Let var: y
          └──Expr: Int:5
          └──Expr: Finish_async
             └──Expr: Block
                └──Expr: Variable: x
                └──Expr: Variable: y
             └──Expr: Block
                └──Expr: Variable: x
                └──Expr: Variable: y
             └──Expr: Objfield: x.f |}]
