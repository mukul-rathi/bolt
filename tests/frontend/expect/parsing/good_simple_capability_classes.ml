open Core
open Print_parsed_ast

let%expect_test "Simple linear class" =
  print_parsed_ast
    " 
    class Foo {
      region linear Bar;
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
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
       └── Method: id
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Effect regions
          └──   Regions: Bar
          └──Body block
             └──Expr: Variable: x
    └──Main block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: x.f
          └──Expr: ObjMethod: x.id
             └──Expr: Int:5 |}]

let%expect_test "Simple thread class" =
  print_parsed_ast
    " 
    class Foo {
      region thread Bar;
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
       └──Regions:
          └──Region: Thread Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
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
      region read Bar;
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
       └──Regions:
          └──Region: Read Bar
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Bool
          └──Regions: Bar
    └──Main block
       └──Expr: Let var: x
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Bool:true
       └──Expr: Objfield: x.f |}]
