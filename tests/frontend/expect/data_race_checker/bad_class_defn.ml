open Core
open Print_data_race_checker_ast

let%expect_test "Class field is of borrowed type" =
  print_data_race_checker_ast
    " 
    class Foo  {
      region linear Bar;
      const borrowed<Foo> f : Bar; // can't have a field with borrowed type 
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect {|
    Foo has a type error:  Field f can't be assigned a borrowed type. |}]

let%expect_test "Class field in region does not have safe capability" =
  print_data_race_checker_ast
    " 
    class Something  {
      region linear Bar;  // not a safe capability
      const int f : Bar;
    }
    class Foo  {
      region read Baz;
      var Something f : Baz; // can't have a field without safe capability
    }
    void main(){
      let x = new Foo()
    }
  " ;
  [%expect
    {|
    Foo has a type error:  Field f can't be in region Baz as it doesn't have capability Safe |}]
