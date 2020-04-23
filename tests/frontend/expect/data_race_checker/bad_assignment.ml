open Core
open Print_data_race_checker_ast

let%expect_test "Assign expr to object without all required capabilities present" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability read Bar, local Baz;
      var int f : Bar, Baz;
    }
    class Something {
      capability linear Else;
      var Foo f : Else;

      void set_f(Foo{Bar} x ) : Else {
        this.f := x
      }
    }
    function void test(Foo x){}
    void main(){
      let x = new Foo();
      let y = new Something();
      y.set_f(consume x)
    }
  " ;
  [%expect
    {|
    Line:11 Position:9 Assigned expression doesn't have all capabilities available |}]

let%expect_test "Assign linear object that hasn't been consumed to field" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability local Bar;
      var Baz f : Bar;
    }
   class Baz {
       capability linear Fa;
       var int g : Fa;
    }
    void main(){
        let x = new Foo();
        let z = new Baz();
        x.f := z // Error: should consume the field

    }
  " ;
  [%expect
    {|
    Line:13 Position:9 Error: Can only assign a linear variable if it has been consumed |}]

let%expect_test "Assign borrowed reference" =
  print_data_race_checker_ast
    " 
   class Foo {
       capability linear Fa;
       var int g : Fa;
    }
    function borrowed Foo f(){
      let x  = new Foo();
      x
    }
    function void test(){
      let y = new Foo(); 
      y := f()  // shouldn't be able to assign a borrowed expression.
    }
    void main(){
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Fa
       └──Field Defn: g
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Fa
    └── Function: f
       └── Return type: Borrowed Class: Foo
       └──Param: Void
       └──Body block
          └──Type expr: Class: Foo
          └──Expr: Let var: _var_x0
             └──Type expr: Class: Foo
             └──Expr: Constructor for: Foo
                └──Type expr: Class: Foo
          └──Expr: Variable: _var_x0
             └──Type expr: Class: Foo
             └── Possible Capabilities:
                └── Possible Capability: Linear Fa
    └── Function: test
       └── Return type: Void
       └──Param: Void
       └──Body block
          └──Type expr: Class: Foo
          └──Expr: Let var: _var_y0
             └──Type expr: Class: Foo
             └──Expr: Constructor for: Foo
                └──Type expr: Class: Foo
          └──Expr: Assign
             └──Type expr: Class: Foo
             └──Expr: Variable: _var_y0
                └──Type expr: Class: Foo
                └── Possible Capabilities:
                   └── Possible Capability: Linear Fa
             └──Expr: Function App
                └──Type expr: Class: Foo
                └──Function: f
                └──()
    └──Main block
       └──Type expr: Void |}]
