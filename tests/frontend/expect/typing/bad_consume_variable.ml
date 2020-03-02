open Core
open Print_typed_ast

let%expect_test "Consume this" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      Foo test() : Bar {
         consume this
      }

    }
    void main(){
    }
  " ;
  [%expect {|
    Line:6 Position:10 Type error - Trying to consume 'this'. |}]

let%expect_test "Consume const field" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      Foo test() : Bar {
         consume this.f
      }

    }
    void main(){
    }
  " ;
  [%expect {|
    Line:6 Position:10 Type error - Trying to consume a const field. |}]
