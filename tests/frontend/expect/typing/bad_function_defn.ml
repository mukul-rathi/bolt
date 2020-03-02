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
    {| Line:11 Position:7 Type mismatch - function expected arguments of type Int * Class: Foo, instead received type Class: Foo * Int |}]

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
    {| Line:11 Position:7 Type mismatch - function expected arguments of type Int, instead received type Class: Foo |}]

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
    {| Line:11 Position:7 Type mismatch - function expected arguments of type Int, instead received type Class: Foo * Class: Foo |}]

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
    Line:14 Position:7 Type error - Function f has duplicate definitions in environment |}]
