open Core
open Print_execution

let%expect_test "Immutable refs in multiple threads" =
  print_execution
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
    let x = 6; 
      {
        let x = new Foo(f:5); (* shadowing in an inner block is okay *)
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
    }
  " ;
  [%expect {| Not supporting this! |}]
