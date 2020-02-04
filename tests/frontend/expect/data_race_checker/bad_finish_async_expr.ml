open Core
open Print_data_race_checker_ast

let%expect_test "Access linear variable from multiple threads" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
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
    Potential data race: no allowed regions for Variable: x
     Allowed capabilities: Linear: false, Thread: false, Read: true, Subordinate: true, Locked: true |}]
