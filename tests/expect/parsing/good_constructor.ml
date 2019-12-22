open Core
open Print_parsed_ast

let%expect_test "Constructor with multiple args" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:4
             └── Field: g
                └──Expr: Int:5
             └── Field: h
                └──Expr: Int:6
       └──Expr: Variable: x |}]
