open Core
open Print_frontend_ir

let%expect_test "Function capability guards correct" =
  print_frontend_ir
    " 
    class Foo  {
      capability locked Bar, subordinate Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo{Bar} y) {
      - (y.f)
    }
    void main(){}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: ThreadLocal ID
       └──Field: Read Lock Counter
       └──Field: Write Lock Counter
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └── Function: f
       └── Return type: Int
       └──Param: Class: Foo y
       └──Body block
          └──Expr: Unary Op: -
             └──Expr: Objfield: y[3]
                └──Lock held: Reader
    └──Main expr |}]

let%expect_test "Function multiple capability guards" =
  print_frontend_ir
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo{Bar,Baz} y) {
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: ThreadLocal ID
       └──Field: Read Lock Counter
       └──Field: Write Lock Counter
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └── Function: f
       └── Return type: Int
       └──Param: Class: Foo y
       └──Body block
          └──Expr: Bin Op: +
             └──Expr: Objfield: y[3]
             └──Expr: Objfield: y[4]
    └──Main expr
       └──Expr: Int:5 |}]

let%expect_test "Method capability guards correct" =
  print_frontend_ir
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;

     int test (Foo{Baz} y) : Bar {
      y.h + this.f
    }
    }
    void main(){5}
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
         └──Field: Int
      └── Function: _Foo__test3Foo
         └── Return type: Int
         └──Param: Class: Foo this
         └──Param: Class: Foo y
         └──Body block
            └──Expr: Bin Op: +
               └──Expr: Objfield: y[5]
               └──Expr: Objfield: this[3]
      └──Main expr
         └──Expr: Int:5 |}]
