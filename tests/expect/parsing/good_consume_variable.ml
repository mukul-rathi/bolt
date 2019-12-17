open Core
open Print_parsed_ast

let%expect_test "Consume variable" =
  print_parsed_ast
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int

    }
    class Choco = thread Late {
      const f : int
    }
    class Bana = read Na {
      const f : int
    }
    thread trait Late {
      require const f : int
    }
    read trait Na {
      require const f : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    {
      {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; (* Consume linear variable *)
        let z = 5;
        let w = consume z; (* Can consume an int *)
        y.h
      };
      {
        let x = new Choco(f:5);
        let y = consume x;
        y
      };
      {
        let x = new Bana(f:5);
        let y = consume x;
        y
      }
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
    └──Class: Choco
       └──CapTrait: Late
          └──Cap: Thread
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
    └──Class: Bana
       └──CapTrait: Na
          └──Cap: Read
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
    └──Trait: Late
       └──Cap: Thread
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
    └──Trait: Na
       └──Cap: Read
       └──Require
          └──Field Defn: f
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
       └──Expr: Block
          └──Expr: Let var: x
             └──Expr: Constructor for: Foo
                └── Field: f
                   └──Expr: Int:4
                └── Field: g
                   └──Expr: Int:5
                └── Field: h
                   └──Expr: Int:6
          └──Expr: Let var: y
             └──Expr: Consume
                └──Expr: Variable: x
          └──Expr: Let var: z
             └──Expr: Int:5
          └──Expr: Let var: w
             └──Expr: Consume
                └──Expr: Variable: z
          └──Expr: Objfield: y.h
       └──Expr: Block
          └──Expr: Let var: x
             └──Expr: Constructor for: Choco
                └── Field: f
                   └──Expr: Int:5
          └──Expr: Let var: y
             └──Expr: Consume
                └──Expr: Variable: x
          └──Expr: Variable: y
       └──Expr: Block
          └──Expr: Let var: x
             └──Expr: Constructor for: Bana
                └── Field: f
                   └──Expr: Int:5
          └──Expr: Let var: y
             └──Expr: Consume
                └──Expr: Variable: x
          └──Expr: Variable: y |}]
