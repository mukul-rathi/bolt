open Core
open Result
open Ast.Ast_types
open Typing.Type_env

let print_error_string = function Ok _ -> "" | Error e -> Error.to_string_hum e

let test_error_if_duplicate_class_defns () =
  let expected_error =
    Fmt.str
      "Line:0 Position:0 Type error - Class Foo has duplicate definitions in environment@."
  in
  let example_class = Parsing.Parsed_ast.TClass (Class_name.of_string "Foo", [], [], []) in
  let result =
    get_class_defn (Class_name.of_string "Foo")
      [example_class; example_class]
      Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let test_error_if_duplicate_class_fields () =
  let expected_error =
    Fmt.str
      "Line:0 Position:0 Type error - Field Baz has duplicate definitions in environment@."
  in
  let example_field = TField (MConst, TEInt, Field_name.of_string "Baz", []) in
  let example_class =
    Parsing.Parsed_ast.TClass
      (Class_name.of_string "Foo", [], [example_field; example_field], []) in
  let result =
    get_class_field (Field_name.of_string "Baz") example_class Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let test_error_if_get_field_of_non_object () =
  let expected_error =
    Fmt.str
      "Line:0 Position:0 Type error - x should be an object, instead is of type Int@."
  in
  let result =
    get_obj_class_defn (Var_name.of_string "x")
      [(Var_name.of_string "x", TEInt)]
      [] Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let () =
  let open Alcotest in
  run "Core Lang Type Env"
    [ ( "Errors"
      , [ test_case "Duplicate Class Definitions" `Quick
            test_error_if_duplicate_class_defns
        ; test_case "Duplicate Class Fields" `Quick test_error_if_duplicate_class_fields
        ; test_case "Access Field of Non-object" `Quick
            test_error_if_get_field_of_non_object ] ) ]
