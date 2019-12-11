open Core
open Print_parsed_ast

let%expect_test "Comments interspersed with code" =
  print_parsed_ast
    " 
    (* This is a comment - it should not be parsed *) 

    let x = 4 in (* Can occur after a line *)
    let y (*Or even midway*) = 5 in
    (* Or before *) x
    end
    (*
    Comments
    Can 
    Span 
    Multiple 
    Lines
    *)
    end
  " ;
  [%expect
    {|
    Program
    └──Expr: Let var: x
       └──Expr: Int:4
       └──Expr: Let var: y
          └──Expr: Int:5
          └──Expr: Variable:x |}]
