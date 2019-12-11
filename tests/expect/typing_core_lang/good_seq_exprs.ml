open Core
open Print_typed_ast

let%expect_test "Seq of exprs" =
  print_typed_ast
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
       └──Type expr: Int
       └──Expr: App
          └──Type expr: Int
          └──Expr: Lambda fun: x
             └──Type expr: Int -> Int
             └──Arg: x
                └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
          └──Expr: Int:4
       └──Expr: App
          └──Type expr: Int
          └──Expr: Lambda fun: x
             └──Type expr: Int -> Int
             └──Arg: x
                └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
          └──Expr: Int:5
       └──Expr: App
          └──Type expr: Int
          └──Expr: Lambda fun: x
             └──Type expr: Int -> Int
             └──Arg: x
                └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
          └──Expr: Int:6 |}]
