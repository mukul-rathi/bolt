open Core
open Print_typed_ast

let%expect_test "Function " =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function Foo f (int z, Foo y) {
      z
    }
    5
  " ;
  [%expect
    {| Type Error for function f: expected return type of Class: Foo but got Int instead |}]

let%expect_test "Function not present" =
  print_typed_ast " 
    f(1) (* No definition for function f *)
  " ;
  [%expect {| Line:2 Position:5 Type error - Function f not defined in environment |}]

let%expect_test "Function arg type mismatch" =
  print_typed_ast " 
    function bool f (bool b ){ b }
    f(5)
  " ;
  [%expect
    {|
    Line:3 Position:5 Type mismatch - function expected arguments of type Bool, instead received type Int |}]
