open Core
open Print_typed_ast

let%expect_test "Assign to field not in class" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const int f : Bar;
    }
    void main(){
      let x = new Foo();
      x.g := 5 // Can't assign to field g as not in class 
    }
  " ;
  [%expect {|
    Line:8 Position:7 Type error - Field g not defined in environment |}]

let%expect_test "Assign wrong type" =
  print_typed_ast
    " 
      class Foo {
        capability linear Bar;
        var int f : Bar;
      }
      void main(){
        let y = new Foo(); 
        let x = new Foo(); 
          x.f := y // Error - try to assign Foo to int 
      }
  " ;
  [%expect
    {|
      Line:9 Position:11 Type error - Assigning type Foo to a field of type Int |}]

let%expect_test "Assign value to const" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const int f : Bar;
    }
    void main(){
      let x = new Foo(); 
      x.f := 5 // Can't assign to const field 
    }
  " ;
  [%expect {|
    Line:8 Position:7 Type error - Assigning expr to a const field. |}]

let%expect_test "Assign value to this" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const int f : Bar;
      Foo test(Foo x) : Bar {
        this := x
      }
    }
    void main(){
      let x = new Foo(); 
      x.test(x)
    }
  " ;
  [%expect {|
    Line:6 Position:9 Type error - Assigning expr to 'this'. |}]
