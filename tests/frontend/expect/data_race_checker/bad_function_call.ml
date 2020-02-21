open Core
open Print_data_race_checker_ast

let%expect_test "Call function on object without all required regions present" =
  print_data_race_checker_ast
    " 
    class Foo {
      region subordinate Bar, thread Baz;
      var int f : Bar, Baz;
    }
    function void test(Foo x){}
    void main(){
      let x = new Foo();
      test(x)
    }
  " ;
  [%expect
    {|
    Line:9 Position:7 Potential data race: Function test's argument region constraints not satisfied. |}]
