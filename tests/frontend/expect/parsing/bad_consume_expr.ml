open Print_parsed_ast

let%expect_test "Consume non-variable" =
  print_parsed_ast " 
    void main(){
        let y1 = consume new Choco(f:5);
   }
  " ;
  [%expect {| Line:3 Position:29: syntax error |}]
