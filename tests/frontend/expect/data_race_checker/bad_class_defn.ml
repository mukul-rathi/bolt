open Core
open Print_data_race_checker_ast

let%expect_test "Class field is of borrowed type" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar;
      const borrowed Foo f : Bar; // can't have a field with borrowed type 
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect {|
    Foo has a type error:  Field f can't be assigned a borrowed type. |}]

let%expect_test "Class field in capability does not have safe mode" =
  print_data_race_checker_ast
    " 
    class Something  {
      capability linear Bar;  // not a safe mode
      const int f : Bar;
    }
    class Foo  {
      capability read Baz;
      var Something f : Baz; // can't have a field without safe mode
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Foo has a type error:  Field f can't be in capability Baz as it doesn't have mode ThreadSafe |}]
