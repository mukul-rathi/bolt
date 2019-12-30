open Typing.Typed_ast
open Runtime_env
open Core
open Result

let exit_scope = [SWAP; POP]

(* Remove bindings when exiting scope of a function / let expression *)

let rec compile_expr = function
  | Integer (_, i) ->
      Ok [PUSH (INT i)] (* Push int on stack so can be used in subsequent instructions *)
  | Block (_, _, exprs) ->
      Result.all (List.map ~f:compile_expr exprs)
      >>| fun expr_codes ->
      let rec concat_codes = function
        | []                 -> []
        | [expr_code]        -> expr_code @ exit_scope
        | expr_code :: codes -> expr_code @ [POP] @ concat_codes codes
        (* Note we discard (POP) the values of all but the last expression *) in
      concat_codes expr_codes
  (* Reduce expr to sub to a value, then subsitute for x (binding) in body, finally get
     rid of binding at end of let expression *)
  | Constructor (_, _, class_name, constructor_args) ->
      compile_constructor_args constructor_args
      >>| fun (args_reduction_code, field_set_code) ->
      args_reduction_code @ [CONSTRUCTOR class_name] @ field_set_code
  (* Reduce arguments to values (so values to assign to fields on top of stack) and then
     create object, so address of object is on top of values at top of stack, then set
     each value on stack - this reduces down to just the address of the constructed object
     on top of stack *)
  | _ -> Error (Error.of_string "Not supporting this! ")

and compile_constructor_args = function
  (* We return two lists of instructions, the first list being instructions to execution
     BEFORE construction of new object - i.e. reducing the expressions being assigned to
     values, and the second list being instructions executed AFTER object created - i.e.
     setting fields to those values. *)
  | [] -> Ok ([], [])
  | ConstructorArg (_, field_name, expr) :: constructor_args ->
      compile_constructor_args constructor_args
      >>= fun (exprs_code, field_set_code) ->
      compile_expr expr
      >>| fun expr_code ->
      (* we reduce expressions to values in left to right order *)
      (expr_code @ exprs_code, field_set_code @ [HEAP_FIELD_SET field_name])

(* note the heap field set instructions are in reverse order of the fields, since this
   corresponds to the LIFO stack ordering *)

let compile_program (Prog (_, _, expr)) = compile_expr expr >>| fun code -> (code, [], [])
