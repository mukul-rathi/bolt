open Core
open Print_data_race_checker_ast

let%expect_test "Assign expr to object without all required regions present" =
  print_data_race_checker_ast
    " 
    class Foo {
      region read Bar, thread Baz;
      var int f : Bar, Baz;
    }
    class Something {
      region linear Else;
      var Foo f : Else;

      void set_f(Foo<Bar> x ) : Else {
        this.f := x
      }
    }
    function void test(Foo x){}
    void main(){
      let x = new Foo();
      let y = new Something();
      y.set_f(consume x)
    }
  " ;
  [%expect
    {|
    Line:11 Position:9 Assigned expression doesn't have all regions available |}]

let%expect_test "Assign linear object that hasn't been consumed to field" =
  print_data_race_checker_ast
    " 
    class Foo {
      region thread Bar;
      var Baz f : Bar;
    }
   class Baz {
       region linear Fa;
       var int g : Fa;
    }
    void main(){
        let x = new Foo();
        let z = new Baz();
        x.f := z // Error: should consume the field

    }
  " ;
  [%expect
    {|
    Line:13 Position:9 Error: Can only assign a linear variable if it has been consumed |}]
