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
