open Core
open Print_typed_ast

let%expect_test "Simple linear class" =
  print_typed_ast
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
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
    └──Expr: Block
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

let%expect_test "Simple thread class" =
  print_typed_ast
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
    └──Expr: Block
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
    └──Expr: Block
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
