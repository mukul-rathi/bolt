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
    let x = new Foo(f:5) in 
      let y = new Foo(f:6) in
        finish{
          async{
            y.f
          }
          async{
            x.f
          }
        }
      end
    end
  " ;
  [%expect {| Line:18 Position:10: syntax error |}]
