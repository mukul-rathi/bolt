open Core
open Print_frontend_ir

let%expect_test "Consume variable" =
  print_frontend_ir
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;

    }
    class Choco {
       region thread Late;
      const int f : Bar;
    }
    class Bana {
       region read Na;
      const int f : Bar;
    }
    void main(){
      if true {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; (* Consume linear variable *)
        let z = 5;
        let w = consume z; (* Can consume an int *)
        y.h
      }
      else {
        if false {
        let x = new Choco(f:5);
        let y = consume x;
        y.f
         }
       else{
         let x = new Bana(f:5);
         let y = consume x.f;
         y
         }
      }
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Field: Int
       └──Field: Int
       └──Field: Int
    └──Class: Choco
       └──Field: Int
    └──Class: Bana
       └──Field: Int
    └──Main expr
       └──Expr: If
          └──Expr: Bool:true
          └──Then block
             └──Expr: Let var: _var_x0
                └──Expr: Constructor for: Foo
                   └── Field: 0
                      └──Expr: Int:4
                   └── Field: 1
                      └──Expr: Int:5
                   └── Field: 2
                      └──Expr: Int:6
             └──Expr: Let var: _var_y0
                └──Expr: Consume
                   └──Expr: Variable: _var_x0
             └──Expr: Let var: _var_z0
                └──Expr: Int:5
             └──Expr: Let var: _var_w0
                └──Expr: Consume
                   └──Expr: Variable: _var_z0
             └──Expr: Objfield: _var_y0[2]
          └──Else block
             └──Expr: If
                └──Expr: Bool:false
                └──Then block
                   └──Expr: Let var: _var_x0
                      └──Expr: Constructor for: Choco
                         └── Field: 0
                            └──Expr: Int:5
                   └──Expr: Let var: _var_y0
                      └──Expr: Consume
                         └──Expr: Variable: _var_x0
                   └──Expr: Objfield: _var_y0[0]
                └──Else block
                   └──Expr: Let var: _var_x0
                      └──Expr: Constructor for: Bana
                         └── Field: 0
                            └──Expr: Int:5
                   └──Expr: Let var: _var_y0
                      └──Expr: Consume
                         └──Expr: Objfield: _var_x0[0]
                   └──Expr: Variable: _var_y0 |}]
