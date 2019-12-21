open Core
open Result
open Typing_core_lang.Type_expr

let print_error_string = function Ok _ -> "" | Error e -> Error.to_string_hum e

let test_error_if_empty_block () =
  let expected_error =
    Fmt.str "Line:0 Position:0 Type error - block of expressions is empty@." in
  let result =
    infer_type_expr [] [] [] (Parsing.Parsed_ast.Block (Lexing.dummy_pos, [])) [] in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let () =
  let open Alcotest in
  run "Type Expr"
    [("Errors", [test_case "Empty block statement" `Quick test_error_if_empty_block])]
