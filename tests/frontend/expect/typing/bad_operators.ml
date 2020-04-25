open Core
open Print_typed_ast

let%expect_test "Arithmetic operators on object" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var bool f : Bar;
    }
    void main () {
    let x = new Foo();
    (x + x);
    (x * 4);
    (x / 4);
    (x - 4);
    (x % 4)
  }
  " ;
  [%expect
    {|
    Line:8 Position:6 Type error - + expected operands of type Int, but they were of type Foo |}]

let%expect_test "Arithmetic operators on bool" =
  print_typed_ast
    " 
    void main(){
     let x = true;
    (x + x);
    (x * 4);
    (x / 4);
    (x - 4);
    (x % 4)
  }
  " ;
  [%expect
    {|
    Line:4 Position:6 Type error - + expected operands of type Int, but they were of type Bool |}]

let%expect_test "Negate a bool" =
  print_typed_ast " 
    void main(){
     let x = true;
    -x
  }
  " ;
  [%expect
    {|
    Line:4 Position:5 Type error - - expected operand of type Int, but it was of type Bool |}]

let%expect_test "Int comparison operators on object" =
  print_typed_ast
    " 
      class Foo {
        capability linear Bar;
         var bool f : Bar;
      }
    void main(){
      let x = new Foo();
      (x < x);
      (x <= x);
      (x > 3);
      (x >= 4)
    }
  " ;
  [%expect
    {|
    Line:8 Position:8 Type error - < expected operands of type Int, but they were of type Foo |}]

let%expect_test "Int comparison operators on bool" =
  print_typed_ast
    " 
    void main(){
      let x = true;
      (x < x);
      (x <= x);
      (x > 3);
      (x >= 4)
    }
  " ;
  [%expect
    {|
    Line:4 Position:8 Type error - < expected operands of type Int, but they were of type Bool |}]

let%expect_test "Bool logical operators on int" =
  print_typed_ast
    " void main(){
      let x = 4;
      let y = 0;
      (x && y);
      (y || x)
    }
  " ;
  [%expect
    {|
    Line:4 Position:8 Type error - && expected operands of type Bool, but they were of type Int |}]

let%expect_test "Bool logical operators on object" =
  print_typed_ast
    " 
      class Foo {
        capability linear Bar;
         var bool f : Bar;
      }
    void main(){
      let x = new Foo();
      let y = new Foo(f:true);
      !x;
      (x && x);
      (x || y)
    }
  " ;
  [%expect
    {|
    Line:9 Position:7 Type error - ! expected operand of type Bool, but it was of type Foo |}]

let%expect_test "Binary operator's operands' types mismatch " =
  print_typed_ast
    " 
      class Foo  {
        capability linear Bar;
         var int f : Bar;
      }
    void main(){
      let x = new Foo();
      let y = true;
      (x && y)
    }
  " ;
  [%expect
    {|
    Line:9 Position:8 Type error - &&'s  operands' types not consistent - they have type Foo and Bool |}]
