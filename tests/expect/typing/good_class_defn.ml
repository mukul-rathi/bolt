open Core
open Print_typed_ast

let%expect_test "Class definition with no methods" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
    └──Expr: Block
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
      int set_f (int x) :Bar {
        this.f:=x
      }
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └── Method: set_f
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Effect regions
          └──   Regions: Bar
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Assign
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Int
    └──Expr: Block
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods call toplevel function" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;

      int get_f () : Bar {
        id( this.f )
      }
    }
    function int id (int x){
        x
    }
    void main(){
      let x = new Foo();
      x.get_f()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └── Method: get_f
          └── Return type: Int
          └──Param: Void
          └── Effect regions
          └──   Regions: Bar
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Function App
                └──Type expr: Int
                └──Function: id
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
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
          └──Expr: () |}]
