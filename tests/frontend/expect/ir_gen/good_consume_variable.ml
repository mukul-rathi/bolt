open Core
open Print_frontend_ir

let%expect_test "Consume linear variable" =
  print_frontend_ir
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
  print_frontend_ir
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
       └──Field: Thread ID
       └──Field: Read Lock Counter
       └──Field: Write Lock Counter
       └──Field: Class: Baz
    └──Class: Baz
       └──Field: Thread ID
       └──Field: Read Lock Counter
       └──Field: Write Lock Counter
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Let var: _var_y0
          └──Expr: Consume
             └──Expr: Objfield: _var_x0[3] |}]
