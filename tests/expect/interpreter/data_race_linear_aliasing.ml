open Core
open Print_execution

let%expect_test "Alias a linear variable" =
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
      let y = x; (* cannot alias linear reference *)
      x
    }
  " ;
  [%expect {|
    Line:2 Position:16: syntax error |}]

let%expect_test "Alias a field of linear variable" =
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
      let y = x.f; (* cannot alias field of a linear reference *)
      x
    }

  " ;
  [%expect {|
    Line:2 Position:16: syntax error |}]
