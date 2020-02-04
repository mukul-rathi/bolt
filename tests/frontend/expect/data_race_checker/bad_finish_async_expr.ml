open Core
open Print_data_race_checker_ast

let%expect_test "Access linear variable from multiple threads" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo();
      finish {
        async{
          x.f := 1
        }
        let y = x;
        y.f
      }

    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Bar
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Finish_async
          └──Type expr: Int
             └── Async Expr Free Vars:
                └── (_var_x0)
             └──Async Expr block
                └──Type expr: Int
                └──Expr: Assign
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) _var_x0.f
                      └──Type expr: Int
                      └──Regions:
                         └──Region: Linear Bar
                      └──Capability allowed?
                         └──Linear: false, Thread: false, Read: false, Subordinate: true, Locked: true
                   └──Expr: Int:1
       └── Current Thread Expr Free Vars:
          └── (_var_x0)
          └──Current thread block
             └──Type expr: Int
             └──Expr: Let var: _var_y0
                └──Type expr: Class: Foo
                └──Expr: Variable: _var_x0
                   └──Type expr: Class: Foo
                   └──Regions:
                      └──Region: Linear Bar
                   └──Capability allowed?
                      └──Linear: false, Thread: false, Read: true, Subordinate: true, Locked: true
             └──Expr: Objfield: (Class: Foo) _var_y0.f
                └──Type expr: Int
                └──Regions:
                   └──Region: Linear Bar
                └──Capability allowed?
                   └──Linear: false, Thread: false, Read: true, Subordinate: true, Locked: true |}]
