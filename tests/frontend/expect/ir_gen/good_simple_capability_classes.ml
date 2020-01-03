open Core
open Print_llvm_ast

let%expect_test "Simple linear class" =
  print_llvm_ast
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
       └──Field: Int f
    └── Function: _Foo_id
       └── Return type: Int
       └──Param: Class: Foo this
       └──Param: Int x
       └──Body block
          └──Expr: Variable: x
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: _var_x0.f
          └──Expr: Function App
             └──Function: _Foo_id
             └──Expr: Variable: _var_x0
             └──Expr: Int:5 |}]

let%expect_test "Simple thread class" =
  print_llvm_ast
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
       └──Field: Int f
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: _var_x0.f
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_llvm_ast
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
       └──Field: Bool f
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Bool:true
       └──Expr: Objfield: _var_x0.f |}]
