open Core
open Print_frontend_ir

let%expect_test "Consume variable" =
  print_frontend_ir
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;

    }
    class Choco {
       region thread Late;
      const int f : Bar;
    }
    class Bana {
       region read Na;
       var int f : Bar;
    }
    void main(){
      if true {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; (* Consume linear variable *)
        let z = 5;
        let w = consume z; (* Can consume an int *)
        y.h
      }
      else {
        if false {
        let x = new Choco(f:5);
        let y = consume x;
        y.f
         }
       else{
         let x = new Bana(f:5);
         let y = consume x.f;
         y
         }
      }
    }
  " ;
  [%expect
    {|
    Line:29 Position:9 Potential data race: no allowed regions for Objfield: (Class: Choco) y.f
     Allowed capabilities: Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true |}]
