open Core
open Print_typed_ast

let%expect_test "Class definition with no methods" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo()
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
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int

      int id (int x){
        x
      }
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo()
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
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └──Expr: Block
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods call toplevel function" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int

      int get_f (){
        id( this.f )
      }
    }
    linear trait Bar {
      require var f : int
    }
    function int id (int x){
        x
    }
    {
      let x = new Foo();
      x.get_f()
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
       └── Function: get_f
          └── Return type: Int
          └── Params: ()
          └──Expr: Block
             └──Type expr: Int
             └──Expr: App
                └──Type expr: Int
                └──Function: id
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └── Function: id
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Expr: Block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: ObjMethod: (Class: Foo) x.get_f
          └──Type expr: Int
          └── Args: () |}]
