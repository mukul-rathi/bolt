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
    let x = 6 in 
      let x = new Foo(f:5) in  (* shadowing over here *)
        let y = 5 in 
        finish{
          async {
            begin
            x ;
            y
            end
          }
          async{
            begin
            x ;
            y
            end
          }
        } ;
        x.f
        end
      end
    end
  " ;
  [%expect {| |}]
