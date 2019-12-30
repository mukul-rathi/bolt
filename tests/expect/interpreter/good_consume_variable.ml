open Core
open Print_execution

let%expect_test "Consume variable" =
  print_execution
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int

    }
    class Choco = thread Late {
      const f : int
    }
    class Bana = read Na {
      const f : int
    }
    thread trait Late {
      require const f : int
    }
    read trait Na {
      require const f : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    {
      {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; (* Consume linear variable *)
        let z = 5;
        let w = consume z; (* Can consume an int *)
        y.h
      };
      {
        let x = new Choco(f:5);
        let y = consume x;
        y
      };
      {
        let x = new Bana(f:5);
        let y = consume x;
        y
      }
    }
  " ;
  [%expect {|
    Line:2 Position:16: syntax error |}]
