open Core
open Print_data_race_checker_ast

let%expect_test "Immutable refs in multiple locals" =
  print_data_race_checker_ast
    " 
    class Foo  {
       capability read Bar, linear Baz;
      const int f : Bar, Baz;
    }
   function int test() {
         5
      }
   void main() {
      let x = new Foo(f:5);
      let y = 5;
      finish{
        // can read aliases in different locals as neither are mutable 
        async {
           x;
          test();
          y
        }
          x;
          y
      };
      x.f
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read Bar
          └──Capability: Linear Baz
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar,Baz
    └── Function: test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Type expr: Int
          └──Expr: Int:5
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
       └──Expr: Let var: _var_y0
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: Finish_async
          └──Type expr: Int
             └── Async Expr Free Vars:
                └── (Foo) _var_x0, Capabilities: Bar
             └──Async Expr block
                └──Type expr: Int
                └──Expr: Variable: _var_x0
                   └──Type expr: Class: Foo
                   └── Possible Capabilities:
                      └── Possible Capability: Read Bar
                      └── Possible Capability: Linear Baz
                └──Expr: Function App
                   └──Type expr: Int
                   └──Function: test
                   └──()
                └──Expr: Variable: _var_y0
                   └──Type expr: Int
       └── Current ThreadLocal Expr Free Vars:
          └── (Foo) _var_x0, Capabilities: Bar
          └──Current thread block
             └──Type expr: Int
             └──Expr: Variable: _var_x0
                └──Type expr: Class: Foo
                └── Possible Capabilities:
                   └── Possible Capability: Read Bar
                   └── Possible Capability: Linear Baz
             └──Expr: Variable: _var_y0
                └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) _var_x0.f
          └──Type expr: Int
          └──Capabilities:
             └──Capability: Read Bar
             └──Capability: Linear Baz |}]
