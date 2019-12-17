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
    class Foo = linear Bar {
      var f : int
    (* Missing closing brace *)
    class Baz = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      x.f:= 5
    end
  " ;
  [%expect {| Line:5 Position:10: syntax error |}]

let%expect_test "Trait defn not terminated" =
  print_parsed_ast
    " 
    class Foo = linear Bar {
      var f : int
    }
    class Baz = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int    
    (* Missing closing brace *)
    let x = new Foo() in 
      x.f:= 5
    end
  " ;
  [%expect {| Line:11 Position:8: syntax error |}]
