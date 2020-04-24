open Core
open Print_typed_ast

let%expect_test "Duplicate class fields" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar;
        var int f : Bar;
    }
    void main(){
      let x = new Foo(g:5); 
      x
    }
  " ;
  [%expect {|
    Foo has a type error:  Duplicate field declarations. |}]

let%expect_test "Duplicate class methods" =
  print_typed_ast
    " 
    class Foo  {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar;
      int test () : Bar{
        this.f 
      }
      int test () : Bar{
        this.g
      }
    }
    void main(){
      let x = new Foo(g:5); 
      x.test()
    }
  " ;
  [%expect {|
    Type error - method test has duplicate definitions in environment |}]

let%expect_test "Duplicate class defns" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
   class Foo { // Not allowed! 
      capability read Baz;
      const int f : Baz;
    }
    class Late { // Fine! 
      capability read Choco;
      const int g : Choco;
    }
    void main(){
      let x = new Foo();
      x.f:= 5
    }
  " ;
  [%expect {|
    Duplicate class declarations. Classes must have distinct names. |}]

let%expect_test "Incorrect method return type" =
  print_typed_ast
    " 
    class Foo  {
      capability read Bar;
      const int f : Bar; 
      int gen() : Bar { // Incorrect method return type 
        new Foo(f:0)
      }
    }
    void main(){
      let x = new Foo(f:5); 
      x.f 
    }
  " ;
  [%expect
    {|
    Type Error for method gen: expected return type of Int but got Class: Foo instead |}]
