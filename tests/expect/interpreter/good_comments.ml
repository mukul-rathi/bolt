open Core
open Print_execution

let%expect_test "Comments interspersed with code" =
  print_execution
    " 
    {
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
  [%expect {|
    Line:2 Position:6: syntax error |}]
