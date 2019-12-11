open Core
open Print_typed_ast

let%expect_test "Class not defined" =
  print_typed_ast " 
    let x = new Foo() in (* Foo not defined! *)
      x 
    end
  " ;
  [%expect {|
    Line:2 Position:13 Type error - Class Foo not defined in environment |}]

let%expect_test "Incorrect constructor field arg type" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    let y = new Foo() in 
      let x = new Foo(f:y, g:5, h:6) in (*Error - try to assign Foo to int in constructor*)
        x
      end
    end
  " ;
  [%expect
    {|
      Line:13 Position:15 Type mismatch - constructor expected argument of type Int, instead received type Class: Foo |}]
