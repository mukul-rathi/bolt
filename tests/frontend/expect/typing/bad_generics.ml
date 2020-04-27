open Core
open Print_typed_ast

let%expect_test "Field uses generic type in non-generic class" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar;
      var T f : Bar;
      void foo() : Bar{}
    }
    void main() {}
  " ;
  [%expect
    {|
      Type error: Using generic type in Foo's field f but not in a generic class |}]

let%expect_test "Method uses generic type in non-generic class" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar;
      var int f : Bar;
      void test(T x): Bar{
      }
      T id(T x): Bar{
        x
      }
    }
    void main() {}
  " ;
  [%expect
    {|
      Type error: Using generic type in Foo's method test but not in a generic class |}]

let%expect_test "Generic used in function" =
  print_typed_ast " 
    function void Foo(T x){
    }
    void main() {}
  " ;
  [%expect {|
      Type error: Using generic type in Foo but not in a generic class |}]

let%expect_test "Generic used in main expression" =
  print_typed_ast " 
    void main() {
      let x : T =  5
    }
  " ;
  [%expect
    {|
      Line:3 Position:7 Type error: Use of generic type but not in a generic class |}]

let%expect_test "Generic instantiated with one type but passed another" =
  print_typed_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
      void setF(Foo<T> x) : Bar{
        this.f := x.f
      }
    }
    void main(){
      let x  = new Foo<int>(f:5);
      let y = new Foo<bool>();
      y.setF(consume x)
    }
  " ;
  [%expect
    {| Line:12 Position:7 Type error - method setF expected arguments of type Foo<Bool>, instead received type Foo<Int> |}]

let%expect_test "Generic instantiated with one type assigned another" =
  print_typed_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
      void setF(T x) : Bar{
        this.f := x
      }
    }
    void main(){
      let x  = new Foo<int>(f:5);
      x: = new Foo<bool>()
    }
  " ;
  [%expect
    {| Line:11 Position:7 Type error - Assigning type Foo<Bool> to a field of type Foo<Int> |}]

let%expect_test "Generic not instantiated with type" =
  print_typed_ast
    " 
    class Foo<T> {
      capability linear Bar;
      var T f : Bar;
    }
    void main() {
      let x = new Foo()
    }
  " ;
  [%expect
    {|
      Line:7 Position:15 Type error - generic class Foo needs to be instantiated with a type parameter |}]

let%expect_test "Non-Generic instantiated with type param" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    void main() {
      let x = new Foo<int>()
    }
  " ;
  [%expect
    {|
      Line:7 Position:15 Type error - non-generic class Foo is being instantiated with a type parameter Int |}]

let%expect_test "Function using generic type" =
  print_typed_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
    }
    function Foo<T> id(Foo<T> x) {
      x
    }
    void main() {
      let x =  new Foo<int>(f:5);
      let y =  new Foo<int>();
      id(consume x)
    }
  " ;
  [%expect
    {|
      Type error: Using generic type in function id but not in a generic class |}]
