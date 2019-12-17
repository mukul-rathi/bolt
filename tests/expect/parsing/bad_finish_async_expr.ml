open Core
open Print_parsed_ast

let%expect_test "No expression after Finish Async" =
  print_parsed_ast
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5);
      let y = new Foo(f:6);
      finish{
        async{
          y.f
        }
        async{
          x.f
        }
      }
    }
  " ;
  [%expect {|
    Line:19 Position:6: syntax error |}]
