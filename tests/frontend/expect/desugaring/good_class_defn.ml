open Core
open Print_desugared_ast

let%expect_test "Class definition with no methods" =
  print_desugared_ast
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
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods" =
  print_desugared_ast
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
          └──Body block
             └──Expr: Assign
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods call toplevel function" =
  print_desugared_ast
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
          └──Body block
             └──Expr: Function App
                └──Type expr: Int
                └──Function: id
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
    └── Function: id
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: ObjMethod: (Class: Foo) _var_x0.get_f
          └──Type expr: Int
          └──() |}]
