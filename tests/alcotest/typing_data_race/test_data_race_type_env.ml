open Core
open Result
open Ast.Ast_types
open Typing_data_race.Data_race_type_env

let print_error_string = function Ok _ -> "" | Error e -> Error.to_string_hum e

let test_error_if_get_body_of_undefined_fn () =
  let expected_error =
    Fmt.str "Line:0 Position:0 Type error - Function f not defined in environment@." in
  let result = get_function_body_expr (Function_name.of_string "f") [] Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let test_error_if_get_body_of_duplicate_function_defns () =
  let expected_error =
    Fmt.str
      "Line:0 Position:0 Type error - Function f has duplicate definitions in environment@."
  in
  let example_function =
    Typing_core_lang.Typed_ast.TFunction
      (Function_name.of_string "f", TEInt, [], Integer (Lexing.dummy_pos, 5)) in
  let result =
    get_function_body_expr (Function_name.of_string "f")
      [example_function; example_function]
      Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let test_error_if_get_type_cap_for_duplicate_class_defns () =
  let expected_error =
    Fmt.str
      "Line:0 Position:0 Type error - Class Foo has duplicate definitions in environment@."
  in
  let example_class =
    Typing_core_lang.Typed_ast.TClass
      (Class_name.of_string "Foo", TCapTrait (Linear, Trait_name.of_string "Bar"), [], [])
  in
  let result =
    get_type_capability
      (TEClass (Class_name.of_string "Foo"))
      [example_class; example_class]
      Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let test_error_if_get_type_cap_for_undefined_class () =
  let expected_error =
    Fmt.str "Line:0 Position:0 Type error - Class Foo not defined in environment@." in
  let result =
    get_type_capability (TEClass (Class_name.of_string "Foo")) [] Lexing.dummy_pos in
  Alcotest.(check string) "same error string" expected_error (print_error_string result)

let () =
  let open Alcotest in
  run "Data Race Type Env"
    [ ( "Errors"
      , [ test_case "Get body expr of duplicately defined function" `Quick
            test_error_if_get_body_of_duplicate_function_defns
        ; test_case "Get body expr of undefined function" `Quick
            test_error_if_get_body_of_undefined_fn
        ; test_case "Get capability of duplicately defined class" `Quick
            test_error_if_get_type_cap_for_duplicate_class_defns
        ; test_case "Get capability of undefined class" `Quick
            test_error_if_get_type_cap_for_undefined_class ] ) ]
