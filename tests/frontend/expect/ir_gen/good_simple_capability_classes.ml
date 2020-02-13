open Core
open Print_frontend_ir

let%expect_test "Simple linear class" =
  print_frontend_ir
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
       └──Field: Thread ID
       └──Field: Lock Counter
       └──Field: Int
    └── Function: _Foo_id
       └── Return type: Int
       └──Param: Class: Foo this
       └──Param: Int x
       └──Body block
          └──Expr: Variable: x
             └──Locked false
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: _var_x0[2]
             └──Locked false
          └──Expr: ObjMethod: _var_x0._Foo_id
             └──Expr: Int:5 |}]

let%expect_test "Simple thread class" =
  print_frontend_ir
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
       └──Field: Thread ID
       └──Field: Lock Counter
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Assign
          └──Expr: Objfield: _var_x0[2]
             └──Locked false
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_frontend_ir
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
       └──Field: Thread ID
       └──Field: Lock Counter
       └──Field: Bool
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
             └── Field: 2
                └──Expr: Bool:true
       └──Expr: Objfield: _var_x0[2]
          └──Locked false |}]
