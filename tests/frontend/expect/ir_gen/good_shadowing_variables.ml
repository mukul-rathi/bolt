open Core
open Print_frontend_ir

let%expect_test "Variable shadowing in different blocks" =
  print_frontend_ir
    "
    class Foo {
      region read Bar;
      const int f : Bar;
    }
    void main(){
    let x = 6; 
      if true {
        let x = new Foo(f:5); (* shadowing in an inner block is okay *)
        let y = -5; 
        finish{
          async {
            x;
            y
          };
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
       └──Field: Int
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Int:6
       └──Expr: If
          └──Expr: Bool:true
          └──Then block
             └──Expr: Let var: _var_x1
                └──Expr: Constructor for: Foo
                   └── Field: 0
                      └──Expr: Int:5
             └──Expr: Let var: _var_y0
                └──Expr: Int:-5
             └──Expr: Finish_async
                   └── Async Expr Free Vars:
                      └── (_var_y0, _var_x1)
                   └──Async Expr block
                      └──Expr: Variable: _var_x1
                      └──Expr: Variable: _var_y0
                   └── Async Expr Free Vars:
                      └── (_var_y0, _var_x1)
                   └──Async Expr block
                      └──Expr: Variable: _var_x1
                      └──Expr: Variable: _var_y0
                └──Current Thread Expr block
                   └──Expr: Variable: _var_x1
             └──Expr: Objfield: _var_x1[0]
          └──Else block
             └──Expr: Int:5 |}]
