open Core
open Print_parsed_ast

let%expect_test "Lambda application" =
  print_parsed_ast " 
    (fun x : int -> x end) 4
  " ;
  [%expect
    {|
    Program
    └──Expr: App
       └──Expr: Fun arg: x
          └──Type expr: Int
          └──Expr: Variable: x
       └──Expr: Int:4 |}]

let%expect_test "Function application" =
  print_parsed_ast " 
    let f = fun x :int -> x end 
    in f 4
    end
  " ;
  [%expect
    {|
    Program
    └──Expr: Let var: f
       └──Expr: Fun arg: x
          └──Type expr: Int
          └──Expr: Variable: x
       └──Expr: App
          └──Expr: Variable: f
          └──Expr: Int:4 |}]

let%expect_test "Function with trait arg" =
  print_parsed_ast
    " 
   class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo(f:5) in 
      let get_field = fun x :linear Bar -> x.f end 
      in get_field x
      end
    end
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Linear
       └──Field Defn: f
          └──Mode: Var
          └──TField: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Var
             └──TField: Int
    └──Expr: Let var: x
       └──Expr: Constructor for: Foo
          └── Field: f
             └──Expr: Int:5
       └──Expr: Let var: get_field
          └──Expr: Fun arg: x
             └──Type expr: CapTrait: Linear Bar
             └──Expr: Objfield: x.f
          └──Expr: App
             └──Expr: Variable: get_field
             └──Expr: Variable: x |}]
