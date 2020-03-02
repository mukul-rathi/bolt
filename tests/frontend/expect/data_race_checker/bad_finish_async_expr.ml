open Core
open Print_data_race_checker_ast

let%expect_test "Access local variable from multiple locals" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability local Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo();
      finish {
        async{
          x.f := 1
        }
        let y = x;
        y.f
      }

    }
  " ;
  [%expect
    {|
    Line:10 Position:11 Potential data race: no allowed capabilities for Objfield: (Class: Foo) _var_x0.f |}]
