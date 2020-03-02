open Core
open Print_data_race_checker_ast

let%expect_test "Access linear region in multiple locals" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      var int f : Bar;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
     finish{
       async{
         x.f // error - as accessing linear region in multiple locals
       }
       x.f 
     }

    }
  " ;
  [%expect
    {|
    Potential data race: Line:9 Position:6 Can't access regions Bar and Bar of object _var_x0 concurrently |}]

let%expect_test "Access regions concurrently that don't share safe state" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar, linear Baz;
      var int f : Bar, Baz; // since Baz and Bar aren't both safe(), we can't access this field concurrently
      int get(): Baz { this.f }
    }
    void main(){
      let x = new Foo(); 
     finish{
       async{
         x.f 
       }
       x.get()
     }

    }
  " ;
  [%expect
    {|
    Potential data race: Line:9 Position:6 Can't access regions Baz and Bar of object _var_x0 concurrently |}]
