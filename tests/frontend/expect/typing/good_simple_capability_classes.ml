open Core
open Print_typed_ast

let%expect_test "Simple linear class" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
      x.f:= x.id(5)

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
       └── Method: id
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: Bar
          └──Body block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign
          └──Type expr: Int
          └──Expr: Objfield: (Class: Foo) x.f
             └──Type expr: Int
          └──Expr: ObjMethod: (Class: Foo) x.id
             └──Type expr: Int
             └──Expr: Int:5 |}]

let%expect_test "Simple local class" =
  print_typed_ast
    " 
    class Foo {
      capability local Bar;
      var int f : Bar;
    }
   void main(){
      let x = new Foo(); 
      x.f:= 5
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: ThreadLocal Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign
          └──Type expr: Int
          └──Expr: Objfield: (Class: Foo) x.f
             └──Type expr: Int
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const bool f : Bar;
    }
    void main(){
      let x = new Foo(f:true); 
      x.f
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Bool
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Bool
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Bool
                └──Expr: Bool:true
       └──Expr: Objfield: (Class: Foo) x.f
          └──Type expr: Bool |}]
