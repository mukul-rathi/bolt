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
          └──Expr: Variable:x
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
          └──Expr: Variable:x
       └──Expr: App
          └──Expr: Variable:f
          └──Expr: Int:4 |}]
