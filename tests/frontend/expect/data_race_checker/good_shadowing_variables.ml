open Core
open Print_data_race_checker_ast

let%expect_test "Variable shadowing in different blocks" =
  print_data_race_checker_ast
    "
    class Foo {
      capability read Bar;
      const int f : Bar;
    }
    void main(){
    let x = 6; 
      if true {
        let x = new Foo(f:5); // shadowing in an inner block is okay *)
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
       └──Expr: Let var: _x0
          └──Type expr: Int
          └──Expr: Int:6
       └──Expr: If
          └──Type expr: Int
          └──Expr: Bool:true
          └──Then block
             └──Type expr: Int
             └──Expr: Let var: _x1
                └──Type expr: Foo
                └──Expr: Constructor for: Foo
                   └──Type expr: Foo
                   └── Field: f
                      └──Type expr: Int
                      └──Expr: Int:5
             └──Expr: Let var: _y0
                └──Type expr: Int
                └──Expr: Int:-5
             └──Expr: Finish_async
                └──Type expr: Foo
                   └── Async Expr Free Vars:
                      └── (Foo) _x1, Capabilities: Bar
                   └──Async Expr block
                      └──Type expr: Int
                      └──Expr: Variable: _x1
                         └──Type expr: Foo
                         └── Possible Capabilities:
                            └── Possible Capability: Read Bar
                      └──Expr: Variable: _y0
                         └──Type expr: Int
                   └── Async Expr Free Vars:
                      └── (Foo) _x1, Capabilities: Bar
                   └──Async Expr block
                      └──Type expr: Int
                      └──Expr: Variable: _x1
                         └──Type expr: Foo
                         └── Possible Capabilities:
                            └── Possible Capability: Read Bar
                      └──Expr: Variable: _y0
                         └──Type expr: Int
             └── Current ThreadLocal Expr Free Vars:
                └── (Foo) _x1, Capabilities: Bar
                └──Current thread block
                   └──Type expr: Foo
                   └──Expr: Variable: _x1
                      └──Type expr: Foo
                      └── Possible Capabilities:
                         └── Possible Capability: Read Bar
             └──Expr: Objfield: (Class: Foo) _x1.f
                └──Type expr: Int
                └──Capabilities:
                   └──Capability: Read Bar
          └──Else block
             └──Type expr: Int
             └──Expr: Int:5 |}]
