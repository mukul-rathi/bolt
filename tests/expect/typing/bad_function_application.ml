open Core
open Print_typed_ast

let%expect_test "Function " =
  print_typed_ast
    " 
    class Foo =  {
      region linear Bar;
      var int f Bar;
    }
    function Foo f (int z, Foo y) {
      z
    }
    void main(){5}
  " ;
  [%expect {| Line:2 Position:16: syntax error |}]

let%expect_test "Function not present" =
  print_typed_ast " 
  void main(){
    f(1) (* No definition for function f *)
  }
  " ;
  [%expect {| Line:3 Position:5 Type error - Function f not defined in environment |}]

let%expect_test "Function arg type mismatch" =
  print_typed_ast
    " 
    function bool f (bool b ){ b }
    void main(){
      f(5)
    }
  " ;
  [%expect
    {|
    Line:4 Position:7 Type mismatch - function expected arguments of type Bool, instead received type Int |}]
