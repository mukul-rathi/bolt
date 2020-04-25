open Core
open Print_typed_ast

let%expect_test "Variable shadowing in different blocks" =
  print_typed_ast
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
       └──Capabilities:
          └──Capability: Read Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Int
          └──Expr: Int:6
       └──Expr: If
          └──Type expr: Int
          └──Expr: Bool:true
          └──Then block
             └──Type expr: Int
             └──Expr: Let var: x
                └──Type expr: Foo
                └──Expr: Constructor for: Foo
                   └──Type expr: Foo
                   └── Field: f
                      └──Type expr: Int
                      └──Expr: Int:5
             └──Expr: Let var: y
                └──Type expr: Int
                └──Expr: Int:-5
             └──Expr: Finish_async
                └──Type expr: Foo
                   └──Async Expr block
                      └──Type expr: Int
                      └──Expr: Variable: x
                         └──Type expr: Foo
                      └──Expr: Variable: y
                         └──Type expr: Int
                   └──Async Expr block
                      └──Type expr: Int
                      └──Expr: Variable: x
                         └──Type expr: Foo
                      └──Expr: Variable: y
                         └──Type expr: Int
                └──Current thread block
                   └──Type expr: Foo
                   └──Expr: Variable: x
                      └──Type expr: Foo
             └──Expr: Objfield: (Class: Foo) x.f
                └──Type expr: Int
          └──Else block
             └──Type expr: Int
             └──Expr: Int:5 |}]
