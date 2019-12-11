open Core
open Print_typed_ast

let%expect_test "Assign to field not in class" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = new Foo() in 
      x.g := 5 (* Can't assign to field g as not in class *)
    end
  " ;
  [%expect {|
    Line:9 Position:7 Type error - Field g not defined in environment |}]

let%expect_test "Assign wrong type" =
  print_typed_ast
    " 
      class Foo = linear Bar {
        var f : int
      }
      linear trait Bar {
        require var f : int
      }
      let y = new Foo() in 
        let x = new Foo() in 
          x.f := y (* Error - try to assign Foo to int *)
        end
      end
  " ;
  [%expect
    {|
      Line:10 Position:11 Type error - Assigning type Class: Foo to a field of type Int |}]

let%expect_test "Assign value to const" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = new Foo() in 
      x.f := 5 (* Can't assign to const field *)
    end
  " ;
  [%expect {|
    Line:9 Position:7 Type error - Assigning expr to a const field. |}]
