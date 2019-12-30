open Core
open Print_typed_ast

let%expect_test "Class not defined" =
  print_typed_ast
    " 
    void main() {
      let x = new Foo() ; (* Foo not defined! *)
      x 
    }
  " ;
  [%expect {|
    Line:3 Position:15 Type error - Class Foo not defined in environment |}]

let%expect_test "Incorrect constructor field arg type" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar;
      const f : int;
      const g : int ; 
      const h : int;
    }
    void main() {
      let y = new Foo();
      let x = new Foo(f:y, g:5, h:6); (*Error - try to assign Foo to int in constructor*)
        x
    }
  " ;
  [%expect {|
      Line:4 Position:16: syntax error |}]
