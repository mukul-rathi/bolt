open Typing_core_lang.Typed_ast
open Runtime_env
open Core
open Result

let exit_scope = [SWAP; POP]

(* Remove bindings when exiting scope of a function / let expression *)

let rec compile_expr = function
  | Integer (_, i) ->
      Ok [PUSH (INT i)] (* Push int on stack so can be used in subsequent instructions *)
  | Variable (_, _, var_name) -> Ok [STACK_LOOKUP var_name]
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
  | ObjField (_, _, var_name, _, field_name) ->
      Ok [STACK_LOOKUP var_name; HEAP_FIELD_LOOKUP field_name]
  (* look up heap address and put on top of stack and then look for field using said
     address *)
  | Assign (_, _, var_name, _, field_name, assigned_expr) ->
      compile_expr assigned_expr
      >>| fun assigned_expr_code ->
      assigned_expr_code
      @ [STACK_LOOKUP var_name; HEAP_FIELD_SET field_name; HEAP_FIELD_LOOKUP field_name]
  (* Reduce assigned expr to value on top of stack, then we get address on top of that
     value on stack, then we set field and then we return the new value of the field *)
  | Constructor (_, _, class_name, constructor_args) ->
      compile_constructor_args constructor_args
      >>| fun (args_reduction_code, field_set_code) ->
      args_reduction_code @ [CONSTRUCTOR class_name] @ field_set_code
  (* Reduce arguments to values (so values to assign to fields on top of stack) and then
     create object, so address of object is on top of values at top of stack, then set
     each value on stack - this reduces down to just the address of the constructed object
     on top of stack *)
  | Consume (_, _, expr) -> (
    match expr with
    | Variable (_, _, var_name) ->
        Ok [STACK_LOOKUP var_name; PUSH NULL; STACK_SET var_name]
    (* We get the existing value bound to the variable on top of stack, then we push null
       on stack to set the variable to null as consumed, leaving the original value on the
       top of the stack *)
    | _ -> Error (Error.of_string "Compile-time error: can only consume variables") )
  | FinishAsync (_, _, async_expr1, async_expr2) ->
      compile_expr async_expr1
      >>= fun async_expr1_code ->
      compile_expr async_expr2
      >>| fun async_expr2_code ->
      (* The first async expression represents computation continuing on this thread,
         whilst the second async expression is computed in another thread (which we
         spawn). We throw away the result of the first async expression and wait (BLOCKED)
         on the completion of this spawned thread before continuing execution of the next
         expression *)
      (SPAWN async_expr2_code :: async_expr1_code) @ [POP; BLOCKED]
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

let compile_program (Prog (_, _, _, expr)) =
  compile_expr expr >>| fun code -> (code, [], [])
