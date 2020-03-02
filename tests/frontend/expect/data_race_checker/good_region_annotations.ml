open Core
open Print_data_race_checker_ast

let%expect_test "Function capability guards correct" =
  print_data_race_checker_ast
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
    void main(){}
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
                └──Capabilities:
                   └──Capability: Locked Bar
    └──Main block
       └──Type expr: Void |}]

let%expect_test "Function multiple capability guards" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo<Bar,Baz> y) {
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
          └──Capability: Read Baz
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
          └──Capabilities: Bar,Baz
       └──Body block
          └──Type expr: Int
          └──Expr: Bin Op: +
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
                └──Capabilities:
                   └──Capability: Linear Bar
             └──Expr: Objfield: (Class: Foo) y.g
                └──Type expr: Int
                └──Capabilities:
                   └──Capability: Linear Bar
                   └──Capability: Read Baz
    └──Main block
       └──Type expr: Int
       └──Expr: Int:5 |}]

let%expect_test "Method capability guards correct" =
  print_data_race_checker_ast
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
