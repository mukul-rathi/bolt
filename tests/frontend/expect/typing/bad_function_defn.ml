open Core
open Print_typed_ast

let%expect_test "Function args in wrong order" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar;
      var int f : Bar;
    }
    function int f (int z, Foo y) {
      z
    }
    void main(){
      let y = new Foo(); 
      f(y, 4) // Error - args in wrong order 
    }
  " ;
  [%expect
    {| Line:11 Position:7 Type error - function f expected arguments of type Int * Class: Foo, instead received type Class: Foo * Int |}]

let%expect_test "Function arg type mismatch" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function int f (int z) {
      z
    }
    void main() {
      let y = new Foo(); 
      f(y) // Error - y is not an int 
    }
  " ;
  [%expect
    {| Line:11 Position:7 Type error - function f expected arguments of type Int, instead received type Class: Foo |}]

let%expect_test "Function too many args" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function int f (int z) {
      z
    }
    void main() {
      let y = new Foo();
      f(y,y) // Error - too many args 
    }
  " ;
  [%expect
    {| Line:11 Position:7 Type error - function f expected arguments of type Int, instead received type Class: Foo * Class: Foo |}]

let%expect_test "Duplicate function definitions" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    function int f (int z) {
      z
    }
    function Foo f (Foo x) { // Error - duplicate function definitions for f 
      x
    }
    void main(){
      let y = new Foo();
      f(5)
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
       └── Return type: Int
       └──Param: z
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Variable: z
             └──Type expr: Int
    └── Function: f
       └── Return type: Class: Foo
       └──Param: x
          └──Type expr: Class: Foo
       └──Body block
          └──Type expr: Class: Foo
          └──Expr: Variable: x
             └──Type expr: Class: Foo
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: y
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Function App
          └──Type expr: Int
          └──Function: f
          └──Expr: Int:5 |}]
