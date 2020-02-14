open Core
open Print_frontend_ir

let%expect_test "Comments interspersed with code" =
  print_frontend_ir
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
    └──Main expr
       └──Expr: Let var: _var_x0
          └──Expr: Int:4
       └──Expr: Let var: _var_y0
          └──Expr: Int:5
       └──Expr: Variable: _var_x0 |}]
