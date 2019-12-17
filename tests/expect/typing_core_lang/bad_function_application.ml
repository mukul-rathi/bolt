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
  [%expect
    {| Line:12 Position:9 Type mismatch - function expected arguments of type Int * Class: Foo, instead received type Class: Foo * Int |}]

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
    let y = new Foo() in 
        f(y) (* Error - y is not an int *)
    end
  " ;
  [%expect
    {| Line:12 Position:9 Type mismatch - function expected arguments of type Int, instead received type Class: Foo |}]

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
    let y = new Foo() in 
        f(y,y) (* Error - too many args *)
    end
  " ;
  [%expect
    {| Line:12 Position:9 Type mismatch - function expected arguments of type Int, instead received type Class: Foo * Class: Foo |}]

let%expect_test "Function not enough args" =
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
    let y = new Foo() in 
        f() (* Error - no args passed in *)
    end
  " ;
  [%expect
    {| Line:12 Position:9 Type mismatch - function expected arguments of type Int, instead received type |}]
