open Core
open Print_frontend_ir

let%expect_test "Variable shadowing in different blocks" =
  print_frontend_ir
    "
    class Foo {
      capability read Bar;
      const int f : Bar;
    }
    void main(){
    let x = 6; 
      if true {
        let x = new Foo(f:5); // shadowing in an inner block is okay 
        let y = -5; 
        finish{
          async {
            x;
            y
          }
          async{
            x;
            y
          }
          x
        };
        x.f
      }
      else {
         5
      }
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
          └──Expr: Int:6
       └──Expr: If
          └──Expr: Bool:true
          └──Then block
             └──Expr: Let var: _x1
                └──Expr: Constructor for: Foo
                   └── Field: 0
                      └──Expr: Int:5
             └──Expr: Let var: _y0
                └──Expr: Int:-5
             └──Expr: Finish_async
                   └── Async Expr Free Vars:
                      └── (_x1)
                   └──Async Expr block
                      └──Expr: Variable: _x1
                      └──Expr: Variable: _y0
                   └── Async Expr Free Vars:
                      └── (_x1)
                   └──Async Expr block
                      └──Expr: Variable: _x1
                      └──Expr: Variable: _y0
                └──Current ThreadLocal Expr block
                   └──Expr: Variable: _x1
             └──Expr: Objfield: _x1[0]
          └──Else block
             └──Expr: Int:5 |}]
