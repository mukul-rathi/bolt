open Core
open Print_data_race_checker_ast

let%expect_test "Consume linear variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;
    }
    void main(){
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x // Consume linear variable 
      }
    }
  " ;
  [%expect {|
    Line:12 Position:6: syntax error |}]

let%expect_test "Consume linear field of variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region thread Bar;
      var Baz f : Bar;
    }
   class Baz {
       region linear Fa;
       var int g : Fa;
    }
    void main(){
        let x = new Foo();
        let y = consume x.f // Consume linear field of variable 

    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Regions:
          └──Region: Thread Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Class: Baz
          └──Regions: Bar
    └──Class: Baz
       └──Regions:
          └──Region: Linear Fa
       └──Field Defn: g
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Fa
    └──Main block
       └──Type expr: Class: Baz
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Let var: _var_y0
          └──Type expr: Class: Baz
          └──Expr: Consume
             └──Expr: Objfield: (Class: Foo) _var_x0.f
                └──Type expr: Class: Baz
                └──Regions:
                   └──Region: Thread Bar |}]
