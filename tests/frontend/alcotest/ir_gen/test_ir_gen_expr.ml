open Core
open Ir_gen.Ir_gen_expr
open Ast.Ast_types

let print_error_string = function Ok _ -> "" | Error e -> Error.to_string_hum e

let test_error_if_gen_method_name_of_non_object () =
  let expected_error =
    Fmt.str "IR Gen error: can't name mangle method foo as variable is of type Int@."
  in
  let result = ir_gen_method_name (Method_name.of_string "foo") TEInt in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let () =
  let open Alcotest in
  run "IR Gen Expr"
    [ ( "Errors"
      , [ test_case "Gen method name of non-object" `Quick
            test_error_if_gen_method_name_of_non_object ] ) ]
