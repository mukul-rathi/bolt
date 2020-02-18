open Core
open Print_data_race_checker_ast

let%expect_test "Assign expr to object without all required regions present" =
  print_data_race_checker_ast
    " 
    class Foo {
      region read Bar, thread Baz;
      var int f : (Bar, Baz);
    }
    class Something {
      region linear Else;
      var Foo f : Else;

      void set_f(Foo x: Bar) : Else {
        this.f := x
      }
    }
    function void test(Foo x){}
    void main(){
      let x = new Foo();
      let y = new Something();
      y.set_f(x)
    }
  " ;
  [%expect
    {|
    Line:11 Position:9 Assigned expression doesn't have all regions available |}]
