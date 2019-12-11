open Core
open Print_typed_ast

let%expect_test "Comments interspersed with code" =
  print_typed_ast
    " 
    (* This is a comment - it should not be typed *) 

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
       └──Type expr: Int
       └──Expr: Int:4
       └──Expr: Let var: y
          └──Type expr: Int
          └──Expr: Int:5
          └──Expr: Variable: x
             └──Type expr: Int |}]
