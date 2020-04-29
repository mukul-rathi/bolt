open Core
open Print_typed_ast

let%expect_test "Class inheriting from non-existent superclass" =
  print_typed_ast
    " 
    class Foo extends Baz {
      capability linear Bar;
      var int f : Bar;
    } 
    void main(){}
  " ;
  [%expect {|
      Type error: superclass Baz for class Foo doesn't exist |}]

let%expect_test "Access field not in class or superclass" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    class Choco extends Foo {
      capability linear Boom;
      var int h : Boom;
    }
    void main() {
      let x = new Baz();
      x.h
    }
  " ;
  [%expect {|
      Line:16 Position:7 Type error - Field h not defined in environment |}]

let%expect_test "Override method of superclass but different return type" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

      int get() : Bar{
        this.f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
      void get() : Bar{
        this.f
      }
    }
    void main() {
      let x = new Baz();
      x.get()
    }
  " ;
  [%expect
    {|
      Type error: Baz overrides method get but has a different return type |}]

let%expect_test "Override method of super-super-class but different return type" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

      int get() : Bar{
        this.f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
      int get() : Bar{
        this.f
      }
    }
     class Banana extends Baz {
      capability linear Nanna;
      var int f : Nanna;

      void get() : Nanna{
        this.f
      }
    }
    void main() {
      let x = new Banana();
      x.get()
    }
  " ;
  [%expect
    {|
      Type error: Banana overrides method get but has a different return type |}]
