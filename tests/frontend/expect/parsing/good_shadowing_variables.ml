open Core
open Print_parsed_ast

let%expect_test "Variable shadowing in different blocks" =
  print_parsed_ast
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
       └──Capabilities:
          └──Capability: Read Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Expr: Let var: x
          └──Expr: Int:6
       └──Expr: If
          └──Expr: Bool:true
          └──Then block
             └──Expr: Let var: x
                └──Expr: Constructor for: Foo
                   └── Field: f
                      └──Expr: Int:5
             └──Expr: Let var: y
                └──Expr: Int:-5
             └──Expr: Finish async
                   └──Async Expr block
                      └──Expr: Variable: x
                      └──Expr: Variable: y
                   └──Async Expr block
                      └──Expr: Variable: x
                      └──Expr: Variable: y
                └──Current thread block
             └──Expr: Objfield: x.f
          └──Else block
             └──Expr: Int:5 |}]
