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
      1
    }
    {
      let x = new Choco(f:5); 
      finish {
        async {
            f(h());
            g(true)
        }
        async{
          let y = new Choco(f:5); 
          let z = new Bana(f:1); 
          let w = new Foo(g:5); 
          w.f := w.id(4)
        }
        };
        x
    }
  " ;
  [%expect {| |}]
