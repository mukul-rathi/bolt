open Core
open Print_execution

let%expect_test "Block of exprs" =
  print_execution
    " 
    function int f (int x){x}
    { 
    f(4);
    f(5);
    f(6)
    }
  " ;
  [%expect {|
    Not supporting this! |}]
