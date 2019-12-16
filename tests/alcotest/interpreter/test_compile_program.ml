open Interpreter
open Compile_program
open Pprint_execution_state
open Core
open Ast.Ast_types
open Typing_core_lang.Typed_ast

let execution_state_res_testable =
  Alcotest.testable
    (fun ppf res ->
      match res with
      | Ok init_state -> pprint_initial_state ppf init_state
      | Error e       -> Fmt.pf ppf "%s@." (Error.to_string_hum e))
    (fun instruction1 instruction2 -> instruction1 = instruction2)

let test_compile_empty_block () =
  Alcotest.(check execution_state_res_testable)
    "same initial"
    (Ok ([], [], []))
    (compile_program (Prog ([], [], Block (Lexing.dummy_pos, TEInt, []))))

let test_compile_error_consume_non_variable () =
  Alcotest.(check execution_state_res_testable)
    "same error"
    (Error (Error.of_string "Compile-time error: can only consume variables"))
    (compile_program
       (Prog ([], [], Consume (Lexing.dummy_pos, TEInt, Integer (Lexing.dummy_pos, 5)))))

let () =
  let open Alcotest in
  run "Compile Program"
    [ ( "Compile expression"
      , [ test_case "Empty Block" `Quick test_compile_empty_block
        ; test_case "Compile error when consuming non variable" `Quick
            test_compile_error_consume_non_variable ] ) ]
