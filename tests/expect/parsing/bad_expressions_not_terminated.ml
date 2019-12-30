open Core
open Print_parsed_ast

let%expect_test "Comment not terminated" =
  print_parsed_ast " 
    (* This comment hasn't been terminated
  " ;
  [%expect
    {| Line:3 Position:3: Lexer - Unexpected EOF - please terminate your comment. |}]

let%expect_test "Class defn not terminated" =
  print_parsed_ast
    " 
    class Foo  {
      region linear Bar;
      var int f : Bar
    (* Missing closing brace *)
    class Baz  {
      region linear Bar;
      var int f : Bar
    }
    void main() {
      let x = new Foo() in 
        x.f:= 5
      end
    }
  " ;
  [%expect {| Line:6 Position:10: syntax error |}]

let%expect_test "If Statement with no else branch" =
  print_parsed_ast " 
    void main() {
      if true {
        3
      }
    }
  " ;
  [%expect {|
    Line:6 Position:6: syntax error |}]
