open Core
open Print_data_race_checker_ast

let%expect_test "Try to assign to a read capability" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability read Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo();
      x.f := 1
    }
  " ;
  [%expect
    {|
    Line:8 Position:7 Potential data race: no allowed capabilities for Objfield: (Class: Foo) _x0.f |}]
