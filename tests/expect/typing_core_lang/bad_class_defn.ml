open Core
open Print_typed_ast

let%expect_test "Duplicate class fields" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
        var f : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
    }
    {
      let x = new Foo(g:5); 
      x
    }
  " ;
  [%expect {|
    Foo has a type error:  Duplicate field declarations. |}]

let%expect_test "Duplicate class defns" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    class Foo = read Chocolate { (* Note different class but has same name - bad! *)
      const g : int
    }
    class Late = read Chocolate { (* Fine! *)
      const g : int
    }
    linear trait Bar {  
      require var f : int
    }
    read trait Chocolate {  
      require const g : int
    }
    {
      let x = new Foo();
      x.f:= 5
    }
  " ;
  [%expect {|
    Duplicate class declarations. Classes must have distinct names. |}]

let%expect_test "Trait/Class mismatched capabilities" =
  print_typed_ast
    " 
   class Foo = thread Bar {
      var f : int
    }
    linear trait Bar {  (* Thread and linear capabilities don't match *)
      require var f : int
    }
    {
      let x = new Foo(); 
      x.f:= 5
    }
  " ;
  [%expect {|
    Foo has a type error:  No matching declarations. |}]

let%expect_test "Class doesn't contain trait's required field" =
  print_typed_ast
    " 
    class Foo = read Bar { 
      const g : int
      (* No field f in class definition even though required by 
        the trait Bar that Foo is using *)
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(); 
      x
    }
  " ;
  [%expect {|
    Foo has a type error:  missing required field: f |}]

let%expect_test "Var field in trait, but const in class" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      const f : int
    }
    linear trait Bar {
      require var f : int
    }
    {
      let x = new Foo(f:5); 
      x.f 
    }
  " ;
  [%expect {|
    Foo has a type error:  missing required field: f |}]

let%expect_test "Const field in trait, but var in class" =
  print_typed_ast
    " 
    class Foo = read Bar {
      var f : int (* var field even though required field is const *)
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      x.f 
    }
  " ;
  [%expect {|
    Foo has a type error:  missing required field: f |}]

let%expect_test "Incorrect method return type" =
  print_typed_ast
    " 
    class Foo = read Bar {
      const f : int 
      int gen(){ (* Incorrect method return type *)
        new Foo(f:0)
      }
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      x.f 
    }
  " ;
  [%expect
    {|
    Type Error for method gen: expected return type of Int but got Class: Foo instead |}]

let%expect_test "Type of field in trait and class don't match" =
  print_typed_ast
    " 
    class Foo = read Bar {
      var f : bool (* bool field even though required field is int *)
    }
    read trait Bar {
      require const f : int
    }
    {
      let x = new Foo(f:5); 
      x.f 
    }
  " ;
  [%expect {|
    Foo has a type error:  missing required field: f |}]
