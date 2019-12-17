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
      let x = new Foo(f:5);
      let y = 5;
      finish{
        (* can read aliases in different threads as neither are mutable *)
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
  [%expect {|
    Not supporting this! |}]
