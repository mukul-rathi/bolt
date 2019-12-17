open Core
open Print_typed_ast

let%expect_test "Trying to call an undefined method" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int 

    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      x.gen() (* No method gen() in Foo *) 
    }
  " ;
  [%expect
    {|
    Line:11 Position:7 Type error - Function gen not defined in environment |}]

let%expect_test "Trying to call a method with wrong args" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int 
      int id(int x){ x}
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      x.id() (* No args passed to x *) 
    }
  " ;
  [%expect
    {|
    Line:11 Position:7 Type mismatch - function expected arguments of type Int, instead received type |}]

let%expect_test "Trying to call a method with arg type_mismatch" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int 
      int id(int x){ x}
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      let y = new Foo(f:6);
      x.id(y) (* Wrong args passed to x *) 
    }
  " ;
  [%expect
    {|
    Line:12 Position:7 Type mismatch - function expected arguments of type Int, instead received type Class: Foo |}]
