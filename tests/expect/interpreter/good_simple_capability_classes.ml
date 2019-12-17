open Core
open Print_execution

let%expect_test "Simple linear class" =
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
      x.f:= 5
    }
  " ;
  [%expect {|
    Not supporting this! |}]

let%expect_test "Simple thread class" =
  print_execution
    " 
    class Foo = thread Bar {
      var f : int
    }
    thread trait Bar {
      require var f : int
    }
    {
      let x = new Foo(); 
      x.f:= 5
    }
  " ;
  [%expect {|
    Not supporting this! |}]

let%expect_test "Simple read class" =
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
      x.f
    }
  " ;
  [%expect {|
    Not supporting this! |}]
