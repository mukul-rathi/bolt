open Core
open Print_typed_ast

let%expect_test "Function region guard doesn't exist" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;
    }
    function int f (Foo y : Banana) { (* Error region Banana doesn't exist! *)
      y.f
    }
    void main(){5}
  " ;
  [%expect {|
    Error: region Banana is not present in Foo |}]

let%expect_test "Function only some of the region guards are correct" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;
    }
    function int f (Foo y : (Bar,Banana)) { (* Error region Bar exists but Banana doesn't exist! *)
      y.f + y.g
    }
    void main(){5}
  " ;
  [%expect {|
    Error: region Banana is not present in Foo |}]

let%expect_test "Method region guard incorrect" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar, read Baz;
      var int f : Bar;
      const int g : (Bar, Baz);
      const int h : Baz;

     int test (Foo y : Chocolate) : Bar {
      y.h + this.f
    }
    }
    void main(){5}
  " ;
  [%expect {|
    Error: region Chocolate is not present in Foo |}]

let%expect_test "Field with incorrect region annotations" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar;
      const int f : Foo; (* not a valid region annotation *)
      const int g : Bar;
    }
    void main(){
      let x = new Foo(f:5)
    }
  " ;
  [%expect {|
    Error: region Foo is not present in Foo |}]

let%expect_test "Region fields have different types" =
  print_typed_ast
    " 
    class Foo  {
      region linear Bar;
      const Foo f : Bar; (* Bar should have field types that are the same *)
      const int g : Bar;
    }
    void main(){
      let x = new Foo(g:5); 
      x
    }
  " ;
  [%expect {|
    Foo has a type error: region Bar should have fields of the same type |}]
