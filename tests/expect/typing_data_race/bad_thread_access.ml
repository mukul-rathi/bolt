open Core
open Print_data_race

let%expect_test "Access a thread variable in other thread" =
  print_data_race
    " 
    class Foo = thread Bar {
      var f : int
    }
    thread trait Bar {
      require var f : int
    }
    let x = new Foo(f:5) in 
      let y = x in 
      finish{

        async{
          x.f 
        }
        async{
          y.f (* cannot read thread alias in different thread*)
        }
      } ;
      x.f
      end
    end
  " ;
  [%expect
    {|
    Line:10 Position:7 Potential data race: thread-local variable accessed from other thread. |}]

let%expect_test "Access an alias of a mutable object in multiple threads" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo(f:5) in 
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
    end
  " ;
  [%expect
    {|
    Line:9 Position:7 Potential data race: threads share mutable variables. |}]
