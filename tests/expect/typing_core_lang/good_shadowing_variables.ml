open Core
open Print_typed_ast

let%expect_test "Immutable refs in multiple threads" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
    let x = 6; 
      {
        let x = new Foo(f:5); (* shadowing in an inner block is okay *)
        let y = 5; 
        finish{
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
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Int
          └──Expr: Int:6
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
