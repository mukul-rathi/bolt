open Interpreter
open Compile_program
open Pprint_execution_state
open Core
open Ast.Ast_types
open Typing.Typed_ast

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

let () =
  let open Alcotest in
  run "Compile Program"
    [("Compile expression", [test_case "Empty Block" `Quick test_compile_empty_block])]
