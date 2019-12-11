open Core
open Print_parsed_ast

let%expect_test "Seq of exprs" =
  print_parsed_ast
    " 
    begin 
    (fun x : int -> x end) 4;
    (fun x : int -> x end) 5;
    (fun x : int -> x end) 6
    end
  " ;
  [%expect
    {|
    Program
    └──Expr: Seq
       └──Expr: App
          └──Expr: Fun arg: x
             └──Type expr: Int
             └──Expr: Variable:x
          └──Expr: Int:4
       └──Expr: App
          └──Expr: Fun arg: x
             └──Type expr: Int
             └──Expr: Variable:x
          └──Expr: Int:5
       └──Expr: App
          └──Expr: Fun arg: x
             └──Type expr: Int
             └──Expr: Variable:x
          └──Expr: Int:6 |}]
