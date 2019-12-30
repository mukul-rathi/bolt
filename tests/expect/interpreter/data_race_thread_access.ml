open Core
open Print_execution

let%expect_test "Access a thread variable in other thread" =
  print_execution
    " 
    class Foo = thread Bar {
      var f : int
    }
    thread trait Bar {
      require var f : int
    }
    {
      let x = new Foo(f:5); 
      let y = x; 
      finish{

        async{
          x.f 
        }
        async{
          y.f (* cannot read thread alias in different thread*)
        }
      } ;
      x.f
    }

  " ;
  [%expect {|
    Line:2 Position:16: syntax error |}]

let%expect_test "Access an alias of a mutable object in multiple threads" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo(f:5); 
      finish{
        (* cannot read same alias in different threads *)
        async{
          x.f 
        }
        async{
          x.f
        }
      } ;
      x.f
    }
  " ;
  [%expect {|
    Line:2 Position:16: syntax error |}]
