open Core
open Print_data_race

let%expect_test "Variable shadowing in different blocks" =
  print_data_race
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    {
    let x = 6; 
      {
        let x = new Foo(f:5); (* shadowing in an inner block is okay *)
        let y = 5; 
        finish{
          async {
            x;
            y
          }
          async{
            x;
            y
          }
        };
        x.f
      }
    }
  " ;
  [%expect {| |}]
