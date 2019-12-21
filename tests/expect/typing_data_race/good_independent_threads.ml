open Core
open Print_data_race

let%expect_test "Consume variable" =
  print_data_race
    " 
    class Foo = linear Bar {
      var f : int
      const g : int  
      const h : int

      int id (int x){
        x
      }

    }
    class Choco = thread Late {
      var f : int
    }
    class Bana = read Na {
      const f : int
    }
    thread trait Late {
      require var f : int
    }
    read trait Na {
      require const f : int
    }
    linear trait Bar {
      require var f : int
      require const g : int  
      require const h : int
    }
    function int f (int x) {
      x
    }
    function bool g (bool x) {
      x
    }
    function int h () {
      10
    }
    {
      let x = new Choco(f:5); 
      finish {
        async {
          if g((4 <= x.f) ){
            f(h())
          }
          else{
            4
          };
            while (x.f < 10){
              x.f := (x.f +1)
          };
           3

        }
        async{
          let y = new Choco(f:5); 
          let z = new Bana(f:1); 
          let w = new Foo(g:5); 
          w.f := w.id(4)
        }
        };
        let y = x
    }
  " ;
  [%expect {| |}]
