open Core
open Print_typed_ast

let%expect_test "Applying an int as a fn" =
  print_typed_ast
    " 
    let x = 5 in 
      x 4 (*Error can't apply 4 to an int *)
    end 
  " ;
  [%expect
    {| Line:3 Position:7 Type mismatch - function type expected but got Int instead |}]

let%expect_test "Function arg type mismatch" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let y = new Foo() in 
      let x = fun z : int -> z end in 
        x y (* Error - y is not an int *)
      end
    end
  " ;
  [%expect
    {| Line:10 Position:9 Type mismatch - function expected argument of type Int, instead received type Class: Foo |}]
