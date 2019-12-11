open Core
open Print_typed_ast

let%expect_test "Duplicate trait defn" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {  
      require var f : int
    }
    read trait Bar {  (* Note different trait but has same name - bad! *)
      require const g : int
    }
    read trait Chocolate {  (* Fine! *)
      require const g : int
    }
    let x = new Foo() in 
      x.f:= 5
    end
  " ;
  [%expect {|
    Duplicate trait declarations. Traits must have distinct names |}]

let%expect_test "Read trait has mutable fields" =
  print_typed_ast
    " 
    class Foo = read Bar {
      var f : int
    }
    read trait Bar {
      require var f : int (* Bad - can't have mutable fields! *)
    }
    let x = new Foo(f:5) in 
      x.f
    end
  " ;
  [%expect {|
    Bar is a read trait but its fields aren't const. |}]
