open Core
open Print_parsed_ast

let%expect_test "Simple linear class" =
  print_parsed_ast
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
             └──Expr: Variable: x
    └──Main block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: x.f
          └──Expr: ObjMethod: x.id
             └──Expr: Int:5 |}]

let%expect_test "Simple local class" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: x.f
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_parsed_ast
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
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Bool:true
       └──Expr: Objfield: x.f |}]
