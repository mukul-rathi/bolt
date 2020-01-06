open Core
open Print_frontend_ir

let%expect_test "Class definition with no methods" =
  print_frontend_ir
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo |}]

let%expect_test "Class definition with methods" =
  print_frontend_ir
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
      int set_f (int x) :Bar {
        this.f:=x
      }
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
    └── Function: _Foo_set_f
       └── Return type: Int
       └──Param: Class: Foo this
       └──Param: Int x
       └──Body block
          └──Expr: Assign
             └──Expr: Objfield: this[0]
             └──Expr: Variable: x
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo |}]

let%expect_test "Class definition with methods call toplevel function" =
  print_frontend_ir
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;

      int get_f () : Bar {
        id( this.f ); this.f
      }
    }
    function void id (int x){
        while(x<0){x:=x+1}
    }
    void main(){
      let x = new Foo();
      x.get_f()
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
    └── Function: id
       └── Return type: Void
       └──Param: Int x
       └──Body block
          └──Expr: While
             └──Expr: Bin Op: <
                └──Expr: Variable: x
                └──Expr: Int:0
             └──Body block
                └──Expr: Assign
                   └──Expr: Variable: x
                   └──Expr: Bin Op: +
                      └──Expr: Variable: x
                      └──Expr: Int:1
    └── Function: _Foo_get_f
       └── Return type: Int
       └──Param: Class: Foo this
       └──Param: Void
       └──Body block
          └──Expr: Function App
             └──Function: id
             └──Expr: Objfield: this[0]
          └──Expr: Objfield: this[0]
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Constructor for: Foo
       └──Expr: Function App
          └──Function: _Foo_get_f
          └──Expr: Variable: _var_x0
          └──Expr: () |}]
