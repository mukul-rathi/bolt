open Core
open Print_execution

let%expect_test "Function application" =
  print_execution " 
    function int f (int x ){ x}
    f(4)
  " ;
  [%expect {|
    Not supporting this! |}]

let%expect_test "Function application with multiple args " =
  print_execution " 
    function int f (int x, int y){ x}
    f (3, 4)
  " ;
  [%expect {|
    Not supporting this! |}]

let%expect_test "Function application with no args " =
  print_execution " 
    function int f ( ){ 4}
    f()
  " ;
  [%expect {|
    Not supporting this! |}]
