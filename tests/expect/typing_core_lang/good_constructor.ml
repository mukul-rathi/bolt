open Core
open Print_typed_ast

let%expect_test "Constructor with multiple args" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    {
      let x = new Foo(f:4, g:5, h:6);
        x
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Linear
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
       └──Field Defn: g
          └──Mode: Const
          └──TField: Int
       └──Field Defn: h
          └──Mode: Const
          └──TField: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
       └──Require
          └──Field Defn: g
             └──Mode: Const
             └──TField: Int
       └──Require
          └──Field Defn: h
             └──Mode: Const
             └──TField: Int
    └──Expr: Block
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Variable: x
          └──Type expr: Class: Foo |}]
