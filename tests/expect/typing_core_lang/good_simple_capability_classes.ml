open Core
open Print_typed_ast

let%expect_test "Simple linear class" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo(); 
      x.f:= 5
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Linear
       └──Field Defn: f
          └──Mode: Var
          └──TField: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign: (Class: Foo) x.f
          └──Type expr: Int
          └──Expr: Int:5 |}]

let%expect_test "Simple thread class" =
  print_typed_ast
    " 
    class Foo = thread Bar {
      var f : int
    }
    thread trait Bar {
      require var f : int
    }
    {
      let x = new Foo(); 
      x.f:= 5
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Thread
       └──Field Defn: f
          └──Mode: Var
          └──TField: Int
    └──Trait: Bar
       └──Cap: Thread
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign: (Class: Foo) x.f
          └──Type expr: Int
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
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
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
       └──Expr: Objfield: (Class: Foo) x.f
          └──Type expr: Int |}]
