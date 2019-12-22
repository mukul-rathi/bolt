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
      var f : bool

      Foo gen() {
      let x = new Foo(); 
      let y = x; (* cannot alias linear reference *)
      x
    }
    }
    linear trait Bar {
      require var f : bool
    }

    {
      let x = new Foo(f:true);
      x.gen()
    }
  " ;
  [%expect {|
    Line:7 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a linear variable in then branch of if statement" =
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
      let z = true;
      let y = if z then x else  new Foo(); (* cannot alias linear reference in any branch *)
      x
    }
  " ;
  [%expect {|
    Line:11 Position:7 Potential data race: aliasing a linear reference |}]

let%expect_test "Alias a linear variable in else branch of if statement" =
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
      let z = true;
      let y = if z then new Foo() else x; (* cannot alias linear reference in any branch *)
      x
    }
  " ;
  [%expect {|
    Line:11 Position:7 Potential data race: aliasing a linear reference |}]
