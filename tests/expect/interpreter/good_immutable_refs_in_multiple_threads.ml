open Core
open Print_execution

let%expect_test "Immutable refs in multiple threads" =
  print_execution
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5);
      let y = 5;
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
      };
      x.f
    }
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
    └──Expr: Block
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
