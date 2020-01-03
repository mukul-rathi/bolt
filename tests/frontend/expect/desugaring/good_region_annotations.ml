open Core
open Print_desugared_ast

let%expect_test "Function region guards correct" =
  print_desugared_ast
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
       └──Regions:
          └──Region: Locked Bar
          └──Region: Subordinate Baz
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar,Baz
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Baz
    └── Function: f
       └── Return type: Int
       └──Param: y
          └──Type expr: Class: Foo
          └──Regions: Bar
       └──Body block
          └──Expr: Unary Op: -
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
    └──Main expr
       └──Expr: Int:5 |}]

let%expect_test "Function multiple region guards" =
  print_desugared_ast
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
       └──Regions:
          └──Region: Linear Bar
          └──Region: Read Baz
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar,Baz
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Baz
    └── Function: f
       └── Return type: Int
       └──Param: y
          └──Type expr: Class: Foo
          └──Regions: Bar,Baz
       └──Body block
          └──Expr: Bin Op: +
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.g
                └──Type expr: Int
    └──Main expr
       └──Expr: Int:5 |}]

let%expect_test "Method region guards correct" =
  print_desugared_ast
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
       └──Regions:
          └──Region: Linear Bar
          └──Region: Read Baz
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar,Baz
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Baz
       └── Method: test
          └── Return type: Int
          └──Param: y
             └──Type expr: Class: Foo
             └──Regions: Baz
          └── Effect regions
          └──   Regions: Bar
          └──Body block
             └──Expr: Bin Op: +
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) y.h
                   └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
    └──Main expr
       └──Expr: Int:5 |}]
