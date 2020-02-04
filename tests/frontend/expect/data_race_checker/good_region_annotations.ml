open Core
open Print_data_race_checker_ast

let%expect_test "Function region guards correct" =
  print_data_race_checker_ast
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
          └──Type expr: Int
          └──Expr: Unary Op: -
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
                └──Regions:
                   └──Region: Locked Bar
                └──Capability allowed?
                   └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
    └──Main block
       └──Type expr: Int
       └──Expr: Int:5 |}]

let%expect_test "Function multiple region guards" =
  print_data_race_checker_ast
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
          └──Type expr: Int
          └──Expr: Bin Op: +
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.f
                └──Type expr: Int
                └──Regions:
                   └──Region: Linear Bar
                └──Capability allowed?
                   └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
             └──Expr: Objfield: (Class: Foo) y.g
                └──Type expr: Int
                └──Regions:
                   └──Region: Linear Bar
                   └──Region: Read Baz
                └──Capability allowed?
                   └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
    └──Main block
       └──Type expr: Int
       └──Expr: Int:5 |}]

let%expect_test "Method region guards correct" =
  print_data_race_checker_ast
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
             └──Type expr: Int
             └──Expr: Bin Op: +
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) y.h
                   └──Type expr: Int
                   └──Regions:
                      └──Region: Read Baz
                   └──Capability allowed?
                      └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                   └──Regions:
                      └──Region: Linear Bar
                   └──Capability allowed?
                      └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
    └──Main block
       └──Type expr: Int
       └──Expr: Int:5 |}]