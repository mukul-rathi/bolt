open Core
open Print_data_race_checker_ast

let%expect_test "Access subordinate variable from outside class" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability subordinate Bar;
      var int f : Bar;
    }
    void main(){
      let x = new Foo();
      x.f
    }
  " ;
  [%expect
    {|
    Line:8 Position:7 Potential data race: no allowed capabilities for Objfield: (Class: Foo) _var_x0.f |}]

let%expect_test "Access subordinate variable from outside class" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability subordinate Bar;
      var int f : Bar;
      void print_field() : Bar{
        finish{
          async{
            this.f // can't access subordinate state in another local
          }

        }
      }
    }
    void main(){
    }
  " ;
  [%expect
    {|
    Line:8 Position:13 Potential data race: no allowed capabilities for Objfield: (Class: Foo) this.f |}]

let%expect_test "Return subordinate state from non-encapsulated method" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability subordinate Bar, locked Baz;
      var int f : Bar;
      var Foo g : Baz;
      Foo return_g() : Baz {
        this.g
      }
    }
    void main(){
    }
  " ;
  [%expect
    {|
    Potential Data Race in Foo's method return_g: Subordinate state returned by non-encapsulated method |}]

let%expect_test "Pass subordinate state to non-encapsulated method" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability subordinate Bar, locked Baz;
      var int f : Bar;
      var Foo g : Baz;
      void return_g(Foo x) : Baz {
        this.g := x
      }
    }
    void main(){
    }
  " ;
  [%expect
    {|
    Potential Data Race in Foo's method return_g: Subordinate arguments passed into non-encapsulated method: x |}]
