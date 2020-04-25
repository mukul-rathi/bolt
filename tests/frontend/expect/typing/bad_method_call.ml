open Core
open Print_typed_ast

let%expect_test "Trying to call an undefined method" =
  print_typed_ast
    " 
    class Foo  {
      capability read Bar;
      const int f : Bar; 

    }
    void main(){
      let x = new Foo(f:5); 
      x.gen() // No method gen() in Foo  
    }
  " ;
  [%expect
    {|
    Line:9 Position:7 Type error - method gen is not defined in environment |}]

let%expect_test "Trying to call a method with wrong args" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const int f : Bar; 
      int id(int x) : Bar { x}
    }
    void main(){
      let x = new Foo(f:5); 
      x.id() // No args passed to x  
    }
  " ;
  [%expect
    {|
    Line:9 Position:7 Type error - method id expected arguments of type Int, instead received type Void |}]

let%expect_test "Trying to call a method with arg type_mismatch" =
  print_typed_ast
    " 
    class Foo {
      capability read Bar;
      const int f : Bar;
      int id(int x) : Bar { x}
    }
    void main(){
      let x = new Foo(f:5); 
      let y = new Foo(f:6);
      x.id(y) // Wrong args passed to x  
    }
  " ;
  [%expect
    {|
    Line:10 Position:7 Type error - method id expected arguments of type Int, instead received type Foo |}]
