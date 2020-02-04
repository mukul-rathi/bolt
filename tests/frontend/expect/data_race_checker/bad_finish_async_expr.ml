open Core
open Print_data_race_checker_ast

let%expect_test "Access linear variable from multiple threads" =
  print_data_race_checker_ast
    " 
    class Foo {
      region read Bar;
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
    Line:10 Position:11 Potential data race: no allowed regions for Objfield: (Class: Foo) x.f
     Allowed capabilities: Linear: false, Thread: false, Read: false, Subordinate: true, Locked: true |}]
