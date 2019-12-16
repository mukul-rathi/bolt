open Core
open Print_data_race

let%expect_test "Immutable refs in multiple threads" =
  print_data_race
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = new Foo(f:5) in 
      let y = 5 in 
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
      } ;
      x.f
      end
    end
  " ;
  [%expect {| |}]
