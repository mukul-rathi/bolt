open Core
open Print_typed_ast

let%expect_test "Lambda application" =
  print_typed_ast " 
    (fun x : int -> x end) 4
  " ;
  [%expect
    {|
    Program
    └──Expr: App
       └──Type expr: Int
       └──Expr: Lambda fun: x
          └──Type expr: Int -> Int
          └──Arg: x
             └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
       └──Expr: Int:4 |}]

let%expect_test "Function application" =
  print_typed_ast " 
    let f = fun x :int -> x end 
    in f 4
    end
  " ;
  [%expect
    {|
    Program
    └──Expr: Let var: f
       └──Type expr: Int
       └──Expr: Lambda fun: x
          └──Type expr: Int -> Int
          └──Arg: x
             └──Type expr: Int
          └──Expr: Variable: x
             └──Type expr: Int
       └──Expr: App
          └──Type expr: Int
          └──Expr: Variable: f
             └──Type expr: Int -> Int
          └──Expr: Int:4 |}]
