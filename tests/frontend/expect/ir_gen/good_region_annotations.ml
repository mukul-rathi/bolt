open Core
open Print_frontend_ir

let%expect_test "Function region guards correct" =
  print_frontend_ir
    " 
    class Foo  {
      region locked Bar, subordinate Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;
    }
    function int f (Foo y : Bar) {
      - (y.f)
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └── Function: f
       └── Return type: Int
       └──Param: Class: Foo y
       └──Body block
          └──Expr: Unary Op: -
             └──Expr: Objfield: y[0]
    └──Main expr
       └──Expr: Int:5 |}]

let%expect_test "Function multiple region guards" =
  print_frontend_ir
    " 
    class Foo  {
      region linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;
    }
    function int f (Foo y : (Bar,Baz)) {
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └── Function: f
       └── Return type: Int
       └──Param: Class: Foo y
       └──Body block
          └──Expr: Bin Op: +
             └──Expr: Objfield: y[0]
             └──Expr: Objfield: y[1]
    └──Main expr
       └──Expr: Int:5 |}]

let%expect_test "Method region guards correct" =
  print_frontend_ir
    " 
    class Foo  {
      region linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;

     int test (Foo y : Baz) : Bar {
      y.h + this.f
    }
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └── Function: _Foo_test
       └── Return type: Int
       └──Param: Class: Foo this
       └──Param: Class: Foo y
       └──Body block
          └──Expr: Bin Op: +
             └──Expr: Objfield: y[2]
             └──Expr: Objfield: this[0]
    └──Main expr
       └──Expr: Int:5 |}]
