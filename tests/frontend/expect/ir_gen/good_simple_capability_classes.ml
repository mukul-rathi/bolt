open Core
open Print_frontend_ir

let%expect_test "Simple linear class" =
  print_frontend_ir
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
      x.f := x.id(5)

    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──VTable [_Foo__idi]
         └──Field: Int
      └── Function: _Foo__idi
         └── Return type: Int
         └──Param: Class: Foo this
         └──Param: Int x
         └──Body block
            └──Expr: Variable: x
      └──Main expr
         └──Expr: Let var: _x0
            └──Expr: Constructor for: Foo
         └──Expr: Assign
            └──Expr: Objfield: _x0[0]
            └──Expr: ObjMethod: _x0[0]
               └──Expr: Int:5 |}]

let%expect_test "Simple local class" =
  print_frontend_ir
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
       └──VTable []
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _x0
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: _x0[0]
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_frontend_ir
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
       └──VTable []
       └──Field: Bool
    └──Main expr
       └──Expr: Let var: _x0
          └──Expr: Constructor for: Foo
             └── Field: 0
                └──Expr: Bool:true
       └──Expr: Objfield: _x0[0] |}]
