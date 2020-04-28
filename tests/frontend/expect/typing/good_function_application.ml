open Core
open Print_typed_ast

let%expect_test "Function application" =
  print_typed_ast " 
    function int f (int x ){ x}
    void main(){
      f(4)
   }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:4 |}]

let%expect_test "Function application with multiple args " =
  print_typed_ast
    " 
    function int f (int x, int y){ x}
    void main(){
       f (3, 4)
   }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Param: y
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:3
          └──Expr: Int:4 |}]

let%expect_test "Function application with no args " =
  print_typed_ast " 
    function int f ( ){ 4}
    void main(){
       f()
   }
  " ;
  [%expect
    {|
    Program
    └── Function: f
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Type expr: Int
          └──Expr: Int:4
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──() |}]

let%expect_test "Function application of borrowed arg " =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function borrowed Foo  f (borrowed Foo x) {
         x
    }
    function int g (borrowed Foo x){
       x.f
    }
    void main(){
       let x = new Foo(f:5);
       g(f(x))
   }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
    └── Function: f
       └── Return type: Borrowed Foo
       └──Borrowed Param: x
          └──Type expr: Foo
       └──Body block
          └──Type expr: Foo
          └──Expr: Variable: x
             └──Type expr: Foo
    └── Function: g
       └── Return type: Int
       └──Borrowed Param: x
          └──Type expr: Foo
       └──Body block
          └──Type expr: Int
          └──Expr: Objfield: (Class: Foo) x.f
             └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
       └──Expr: Function App
          └──Type expr: Int
          └──Function: g
          └──Expr: Function App
             └──Type expr: Foo
             └──Function: f
             └──Expr: Variable: x
                └──Type expr: Foo |}]
