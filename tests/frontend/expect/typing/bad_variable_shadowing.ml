open Core
open Print_typed_ast

let%expect_test "Variable shadowing in same block" =
  print_typed_ast
    " 
    class Foo  {
      region read Bar;
      const int f : Bar;
    }
   void main() {
      let x = 6; 
      let x = new Foo(f:5); // shadowing in an same block not allowed 
      let y = 5; 
      finish{
        async {
          x;
          y
        }
        async{
          x;
          y
        }
      };
      x.f
    }
  " ;
  [%expect
    {|
    Line:6 Position:16 Type error: Duplicate variable declarations in same block. |}]
