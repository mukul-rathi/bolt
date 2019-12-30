open Core
open Print_typed_ast

let%expect_test "Comments interspersed with code" =
  print_typed_ast
    " 
    void main(){
    (* This is a comment - it should not be parsed *) 
    let x = 4;(* Can occur after a line *)
    let y (*Or even midway*) = 5;
    (* Or before *) x
    (*
    Comments
    Can 
    Span 
    Multiple 
    Lines
    *)
    }
  " ;
  [%expect
    {|
    Program
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Int
          └──Expr: Int:4
       └──Expr: Let var: y
          └──Type expr: Int
          └──Expr: Int:5
       └──Expr: Variable: x
          └──Type expr: Int |}]
