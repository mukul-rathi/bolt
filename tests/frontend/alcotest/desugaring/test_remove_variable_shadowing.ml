open Core
open Desugaring.Remove_variable_shadowing
open Ast.Ast_types
open Desugaring.Desugared_ast

let print_error_string = function Ok _ -> "" | Error e -> Error.to_string_hum e

let test_error_if_var_not_in_var_map () =
  let expected_error =
    Fmt.str "Error: no unique var name for (potentially) shadowed variable foo@." in
  let result =
    remove_var_shadowing_expr
      (Identifier (Lexing.dummy_pos, Variable (TEVoid, Var_name.of_string "foo")))
      [] in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let () =
  let open Alcotest in
  run "Remove Variable Shadowing"
    [("Errors", [test_case "Var not in var map" `Quick test_error_if_var_not_in_var_map])]
