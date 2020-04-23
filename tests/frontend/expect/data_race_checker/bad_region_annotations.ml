open Core
open Print_data_race_checker_ast

let%expect_test "Function capability guard doesn't exist" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo{Banana} y) { // Error capability Banana doesn't exist! 
      y.f
    }
    void main(){5}
  " ;
  [%expect {|
    Error: capability Banana is not present in Foo |}]

let%expect_test "Function only some of the capability guards are correct" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;
    }
    function int f (Foo{Bar,Banana} y ) { // Error capability Bar exists but Banana doesn't exist! 
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect {|
    Error: capability Banana is not present in Foo |}]

let%expect_test "Method capability guard incorrect" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar, read Baz;
      var int f : Bar;
      const int g : Bar, Baz;
      const int h : Baz;

     int test (Foo{Chocolate} y) : Bar {
      y.h + this.f
    }
    }
    void main(){5}
  " ;
  [%expect {|
    Error: capability Chocolate is not present in Foo |}]

let%expect_test "Field with incorrect capability annotations" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar;
      const int f : Foo; // not a valid capability annotation 
      const int g : Bar;
    }
    void main(){
      let x = new Foo(f:5)
    }
  " ;
  [%expect {|
    Error: capability Foo is not present in Foo |}]

let%expect_test "Capability fields have different types" =
  print_data_race_checker_ast
    " 
    class Foo  {
      capability linear Bar;
      const Foo f : Bar; // Bar should have field types that are the same 
      const int g : Bar;
    }
    void main(){
      let x = new Foo(g:5); 
      x
    }
  " ;
  [%expect
    {|
    Foo has a type error: capability Bar should have fields of the same type |}]
