open Core
open Print_execution

let%expect_test "Equivalent constructor expressions" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo();
      let y = new Foo; (* This is equivalent *)
      x.f := 5
    }
  " ;
  [%expect {|
    Not supporting this! |}]

let%expect_test "Constructor with multiple args" =
  print_execution
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    {
      let x = new Foo(f:4, g:5, h:6);
        x
    }
  " ;
  [%expect {|
    Not supporting this! |}]
