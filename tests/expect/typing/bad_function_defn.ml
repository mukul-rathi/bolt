open Core
open Print_typed_ast

let%expect_test "Function args in wrong order" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function int f (int z, Foo y) {
      z
    }
    let y = new Foo() in 
        f(y, 4) (* Error - args in wrong order *)
    end
  " ;
  [%expect {| Line:11 Position:25: syntax error |}]

let%expect_test "Function arg type mismatch" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function int f (int z) {
      z
    }
    {
      let y = new Foo(); 
      f(y) (* Error - y is not an int *)
    }
  " ;
  [%expect
    {| Line:13 Position:7 Type mismatch - function expected arguments of type Int, instead received type Class: Foo |}]

let%expect_test "Function too many args" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function int f (int z) {
      z
    }
    {
      let y = new Foo();
      f(y,y) (* Error - too many args *)
    }
  " ;
  [%expect
    {| Line:13 Position:7 Type mismatch - function expected arguments of type Int, instead received type Class: Foo * Class: Foo |}]

let%expect_test "Duplicate function definitions" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function int f (int z) {
      z
    }
    function linear Bar f (Foo x) { (* Error - duplicate function definitions for f *)
      x
    }
    {
      let y = new Foo();
      f(5)
    }
  " ;
  [%expect
    {|
    Type Error for function f: expected return type of CapTrait: Linear Bar but got Class: Foo instead |}]
