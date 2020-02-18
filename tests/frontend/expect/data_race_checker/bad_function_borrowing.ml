open Core
open Print_data_race_checker_ast

let%expect_test "Pass in linear arguments twice to function" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
    }
    function void test2(Foo x, Foo y){
    }
    void main() {
      let x = new Foo ();
      test2(x,x) // not allowed!
    }
  " ;
  [%expect {|
    Line:10 Position:7 Linear arguments are duplicated |}]

let%expect_test "Pass in linear object to own method" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
      void test(Foo y) : Bar {
      }
    }
    void main() {
      let x = new Foo ();
      x.test(x) // not allowed!
    }
  " ;
  [%expect
    {|
    Line:10 Position:7 One of linear object _var_x0's method's arguments aliases it |}]
