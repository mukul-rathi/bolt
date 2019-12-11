open Core
open Print_data_race

let%expect_test "Alias a linear variable" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      let y = x in (* cannot alias linear reference *)
        x
      end
    end
  " ;
  [%expect {|
    Line:9 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a field of linear variable" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo(f:5) in 
      let y = x.f in (* cannot alias field of a linear reference *)
        x
      end
    end
  " ;
  [%expect {|
    Line:9 Position:7 Potential data race: aliasing a linear reference |}]
