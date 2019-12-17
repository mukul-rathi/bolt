open Core
open Print_parsed_ast

let%expect_test "Variable shadowing in different blocks" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Int:6
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
