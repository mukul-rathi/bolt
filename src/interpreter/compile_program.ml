open Typed_ast
open Runtime_env
open Core
open Result

let exit_scope = [SWAP; POP]

(* Remove bindings when exiting scope of a function / let expression *)

let rec compile_expr = function
  | Integer (_, i)                                          -> Ok [PUSH (INT i)]
  | Variable (_, _, var_name)                               -> Ok [STACK_LOOKUP var_name]
  | Lambda (_, _, arg_var, _, body)                         ->
      compile_expr body
      >>| fun body_code -> [MK_CLOSURE ((BIND arg_var :: body_code) @ exit_scope)]
  | App (_, _, func, arg)                                   ->
      compile_expr func
      >>= fun func_code ->
      compile_expr arg >>| fun arg_code -> func_code @ arg_code @ [APPLY] @ exit_scope
  (* reduce func first to closure value (as left-to-right evaluation ) then reduce
     argument to value (as call-by-value) and then apply closure to arg, and finally get
     rid of closure's env *)
  | Seq (_, _, exprs)                                       ->
      List.fold ~init:(Ok [])
        ~f:(fun acc expr ->
          acc
          >>= fun acc_code -> compile_expr expr >>| fun expr_code -> acc_code @ expr_code)
        exprs
  (* evaluate left to right *)
  | Let (_, _, var_name, expr_to_sub, body_expr)            ->
      compile_expr expr_to_sub
      >>= fun expr_to_sub_code ->
      compile_expr body_expr
      >>| fun body_expr_code ->
      expr_to_sub_code @ [BIND var_name] @ body_expr_code @ exit_scope
  (* Reduce expr to sub to a value, then subsitute for x (binding) in body, finally get
     rid of binding at end of let expression *)
  | ObjField (_, _, var_name, _, field_name)                ->
      Ok [STACK_LOOKUP var_name; HEAP_FIELD_LOOKUP field_name]
  (* look up heap address and put on top of stack and then look for field using said
     address *)
  | Assign (_, _, var_name, _, field_name, assigned_expr)   ->
      compile_expr assigned_expr
      >>| fun assigned_expr_code ->
      assigned_expr_code @ [STACK_LOOKUP var_name; HEAP_FIELD_SET field_name]
  (* Reduce assigned expr to value on top of stack, then we get address on top of that
     value on stack, then we set field *)
  | Constructor (_, _, class_name, constructor_args)        ->
      compile_constructor_args constructor_args
      >>| fun (args_reduction_code, field_set_code) ->
      args_reduction_code @ [CONSTRUCTOR class_name] @ field_set_code
  (* Reduce arguments to values (so values to assign to fields on top of stack) and then
     create object, so address of object is on top of values at top of stack, then set
     each value on stack - this reduces down to just the address of the constructed
     object on top of stack *)
  | Consume (_, _, expr)                                    -> (
    match expr with
    | Variable (_, _, var_name) ->
        Ok [STACK_LOOKUP var_name; PUSH NULL; STACK_SET var_name]
    (* We get the existing value bound to the variable on top of stack, then we push null
       on stack to set the variable to null as consumed, leaving the original value on
       the top of the stack *)
    | _                         -> Error
                                     (Error.of_string
                                        "Compile-time error: can only consume variables")
    )
  | FinishAsync (_, _, async_expr1, async_expr2, next_expr) ->
      compile_expr async_expr1
      >>= fun async_expr1_code ->
      compile_expr async_expr2
      >>= fun async_expr2_code ->
      compile_expr next_expr
      >>| fun next_expr_code ->
      (SPAWN async_expr2_code :: async_expr1_code) @ [POP; BLOCKED] @ next_expr_code

(* We pop the value of this thread's computation as *)
and compile_constructor_args = function
  | []                                                            -> Ok ([], [])
  | ConstructorArg (_, field_name, expr) :: constructor_args ->
      compile_constructor_args constructor_args
      >>= fun (exprs_code, field_set_code) ->
      compile_expr expr
      >>| fun expr_code ->
      (expr_code @ exprs_code, field_set_code @ [HEAP_FIELD_SET field_name])

let compile_program (Prog (_, _, expr)) = compile_expr expr >>| fun code -> (code, [], [])
