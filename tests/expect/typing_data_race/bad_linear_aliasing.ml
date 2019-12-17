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
    {
      let x = new Foo(); 
      let y = x; (* cannot alias linear reference *)
      x
    }
  " ;
  [%expect {|
    Line:10 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a field of linear variable" =
  print_data_race
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
    Line:10 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a linear variable in a function" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    function Foo test() {
      let x = new Foo(); 
      let y = x; (* cannot alias linear reference *)
      x
    }
    {
      test()
    }
  " ;
  [%expect {|
    Line:10 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a linear variable in a method" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int

      Foo gen() {
      let x = new Foo(); 
      let y = x; (* cannot alias linear reference *)
      x
    }
    }
    linear trait Bar {
      require var f : int
    }

    {
      let x = new Foo();
      x.gen()
    }
  " ;
  [%expect {|
    Line:7 Position:7 Potential data race: aliasing a linear reference |}]
