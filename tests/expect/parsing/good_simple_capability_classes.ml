open Core
open Print_parsed_ast

let%expect_test "Simple linear class" =
  print_parsed_ast
    " 
    class Foo = linear Bar {
      var f : int
      int id (int x){ x}
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo(); 
      x.f:= x.id(5)

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
       └── Function: id
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └──Expr: Block
             └──Expr: Variable: x
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └──Expr: Block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
       └──Expr: Assign: x.f
          └──Expr: ObjMethod: x.id
             └──Expr: Int:5 |}]

let%expect_test "Simple thread class" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
       └──Expr: Assign: x.f
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_parsed_ast
    " 
    class Foo = read Bar {
      const f : bool
    }
    read trait Bar {
      require const f : bool
    }
    {
      let x = new Foo(f:true); 
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
          └──TField: Bool
    └──Trait: Bar
       └──Cap: Read
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Bool
    └──Expr: Block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Bool:true
       └──Expr: Objfield: x.f |}]
