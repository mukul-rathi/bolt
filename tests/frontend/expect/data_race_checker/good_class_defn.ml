open Core
open Print_data_race_checker_ast

let%expect_test "Class definition with no methods" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
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
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
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
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
       └── Method: set_f
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: Bar
          └──Body block
             └──Type expr: Int
             └──Expr: Assign
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                   └──Capabilities:
                      └──Capability: Linear Bar
                └──Expr: Variable: x
                   └──Type expr: Int
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo |}]

let%expect_test "Class definition with methods call toplevel function" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
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
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
       └── Method: get_f
          └── Return type: Int
          └──Param: Void
          └── Used capabilities
          └──   Capabilities: Bar
          └──Body block
             └──Type expr: Int
             └──Expr: Function App
                └──Type expr: Int
                └──Function: id
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                   └──Capabilities:
                      └──Capability: Linear Bar
    └── Function: id
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: ObjMethod: (Class: Foo) _var_x0.get_f
          └──Type expr: Int
          └──() |}]
