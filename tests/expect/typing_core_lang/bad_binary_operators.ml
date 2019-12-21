open Core
open Print_typed_ast

let%expect_test "Arithmetic operators on object" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : bool
    }
    linear trait Bar {
      require var f: bool
    }
    {
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
    Line:10 Position:5 Type error - + expected operands of type Int, but they were of type Class: Foo |}]

let%expect_test "Arithmetic operators on bool" =
  print_typed_ast
    " 
    {
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
    Line:4 Position:5 Type error - + expected operands of type Int, but they were of type Bool |}]

let%expect_test "Int comparison operators on object" =
  print_typed_ast
    " 
      class Foo = linear Bar {
         var f : bool
      }
      linear trait Bar {
         require var f: bool
      }
    {
      let x = new Foo();
      (x < x);
      (x <= x);
      (x > 3);
      (x >= 4)
    }
  " ;
  [%expect
    {|
    Line:10 Position:7 Type error - < expected operands of type Int, but they were of type Class: Foo |}]

let%expect_test "Int comparison operators on bool" =
  print_typed_ast
    " 
    {
      let x = true;
      (x < x);
      (x <= x);
      (x > 3);
      (x >= 4)
    }
  " ;
  [%expect
    {|
    Line:4 Position:7 Type error - < expected operands of type Int, but they were of type Bool |}]

let%expect_test "Boolean operators" =
  print_typed_ast "
      ( (true || false) && false)
  " ;
  [%expect
    {|
    Program
    └──Expr: Bin Op: &&
       └──Type expr: Bool
       └──Expr: Bin Op: ||
          └──Type expr: Bool
          └──Expr: Bool:true
          └──Expr: Bool:false
       └──Expr: Bool:false |}]

let%expect_test "Bool logical operators on int" =
  print_typed_ast
    " {
      let x = 4;
      let y = 0;
      (x && y);
      (y || x)
    }
  " ;
  [%expect
    {|
    Line:4 Position:7 Type error - && expected operands of type Bool, but they were of type Int |}]

let%expect_test "Bool logical operators on object" =
  print_typed_ast
    " 
      class Foo = linear Bar {
         var f : bool
      }
      linear trait Bar {
         require var f: bool
      }
    {
      let x = new Foo();
      let y = new Foo(f:true);
      (x && x);
      (x || y)
    }
  " ;
  [%expect
    {|
    Line:11 Position:7 Type error - && expected operands of type Bool, but they were of type Class: Foo |}]

let%expect_test "Binary operator's operands' types mismatch " =
  print_typed_ast
    " 
      class Foo = linear Bar {
         var f : bool
      }
      linear trait Bar {
         require var f: bool
      }
    {
      let x = new Foo();
      let y = true;
      (x && y)
    }
  " ;
  [%expect
    {|
    Line:11 Position:7 Type error - &&'s  operands' types not consistent - they have type Class: Foo and Bool |}]
