open Core
open Print_typed_ast

let%expect_test "Function capability guards correct" =
  print_typed_ast
    " 
    class Foo  {
      capability locked Bar, subordinate Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo<Bar> y) {
      - (y.f)
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Locked Bar
          └──Capability: Subordinate Baz
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: g
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar,Baz
       └──Field Defn: h
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Baz
    └── Function: f
       └── Return type: Int
       └──Param: y
          └──Type expr: Class: Foo
          └──Capabilities: Bar
       └──Body block
          └──Type expr: Int
          └──Expr: Unary Op: -
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Int:5 |}]

let%expect_test "Function multiple capability guards" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;
    }
    function int f (Foo y : (Bar,Baz)) {
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect {|
    Line:5 Position:22: syntax error |}]

let%expect_test "Method capability guards correct" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;

     int test (Foo y : Baz) : Bar {
      y.h + this.f
    }
    }
    void main(){5}
  " ;
  [%expect {|
    Line:5 Position:22: syntax error |}]
