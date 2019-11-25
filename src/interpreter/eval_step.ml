open Runtime_env
open Core
open Ast_types
open Result

let remove_thread (TThread(thread_id, expr, stack)) thread_pool = List.filter ~f:(fun (TThread(tid,_,_)) -> not(tid = thread_id)) thread_pool

let replace_thread (TThread(thread_id, expr, stack)) thread_pool = 
  (TThread(thread_id, expr, stack))::(remove_thread thread_pool)

let get_thread thread_id thread_pool = 
  (List.filter ~f:(fun (TThread(tid,_,_)) -> not(tid = thread_id)) thread_pool) 
  |> (function 
      | [] -> Error (Error.of_string (Fmt.str "Runtime error: Thread with id: %s not present.@." (string_of_thread_id thread_id)) )
      | [thread] -> Ok thread
      | _ -> Error (Error.of_string (Fmt.str "Runtime error: Duplicate threads with id: %s  present.@." (string_of_thread_id thread_id)) )
    )

let has_thread_completed thread_id thread_pool = get_thread_by_id thread_id thread_pool
  >>= fun (TTrait(_, expr, _)) -> (match expr with | (Value v) -> true | _ -> false)

(* Helper function in order for us to step in sub-exprs recursively for congruence rules *)
let step_expr expr stack heap thread_pool = (match expr with 
    | Value v              ->  Ok (expr,stack,heap,thread_pool) (* No more computation to be done *)
    | Blocked _ -> Error (Error.of_string "Runtime error: thread blocked but no computation after.") (* This should never happen - if you are blocked, it means you have unfinished computation *)
    | Seq []   -> Error (Error.of_string "Runtime error: Empty sequence of computations.") 
    | Seq ((Blocked tid)::exprs)   -> (has_thread_completed tid thread_pool) (* check if thread you are waiting on has finished yet *)>>= (function |true -> Ok ((Seq exprs), stack, heap, (remove_thread tid thread_pool)) (* if it has, remove it from the thread pool and carry on with your computation *) | false -> Ok ((Seq ((Blocked tid)::exprs)), stack , heap, thread_pool))      (* If it hasn't, do nothing (still blocked) *)
    | Seq (Value v :: [])            -> Ok ((Value v) ,stack, heap, thread_pool) (*We've reached end of computation - got a value *)
    | Seq (Value _ :: exprs)            -> Ok ((Seq exprs) ,stack, heap, thread_pool) (* throw away value and move onto next expr in sequence *)
    | Seq (sub_expr::exprs)            -> (step_expr sub_expr stack heap thread_pool) >>| fun (sub_expr',stack,heap',thread_pool') -> ((Seq (sub_expr'::exprs) ),stack,heap',thread_pool')
    | TypedExpr typed_expr -> (
        match typed_expr with
        | Integer (_, i) -> Ok ((Value(INT i)),stack,heap,thread_pool)
        | Variable (_, _, var_name)                                                 -> Var_name
                                                                                       .to_string
                                                                                         var_name
        | Lambda (_, _, arg_var, arg_type, body)                                    ->
          Fmt.str "Fun (%s:%s) -> (%s)" (Var_name.to_string arg_var)
            (string_of_type arg_type)
            (string_of_expr (TypedExpr body))
        | App (_, _, func, arg)                                                     ->
          Fmt.str "(%s) (%s)"
            (string_of_expr (TypedExpr func))
            (string_of_expr (TypedExpr arg))
        | Seq (_, _, typed_exprs)                                                   ->
          Fmt.str "Begin: %s :End"
            (String.concat ~sep:";"
               (List.map
                  ~f:(fun typed_expr -> string_of_expr (TypedExpr typed_expr))
                  typed_exprs))
        | Let (_, _, var_name, typed_expr_to_sub, body_typed_expr)                  ->
          Fmt.str "Let %s = %s in %s end" (Var_name.to_string var_name)
            (string_of_expr (TypedExpr typed_expr_to_sub))
            (string_of_expr (TypedExpr body_typed_expr))
        | ObjField (_, _, var_name, _, field_name)                                  ->
          Fmt.str "%s.%s" (Var_name.to_string var_name) (Field_name.to_string field_name)
        | Assign (_, _, var_name, _, field_name, assigned_typed_expr)               ->
          Fmt.str "%s.%s := %s" (Var_name.to_string var_name)
            (Field_name.to_string field_name)
            (string_of_expr (TypedExpr assigned_typed_expr))
        | Constructor (_, _, class_name, constructor_args)                          ->
          Fmt.str "new %s(%s)"
            (Class_name.to_string class_name)
            (String.concat ~sep:","
               (List.map ~f:string_of_constructor_arg constructor_args))
        | Consume (_, _, typed_expr)                                                ->
          Fmt.str "consume %s" (string_of_expr (TypedExpr typed_expr))
        | FinishAsync (_, _, async_typed_expr1, async_typed_expr2, next_typed_expr) ->
          Fmt.str "finish{ async{ %s } async { %s } } ; %s "
            (string_of_expr (TypedExpr async_typed_expr1))
            (string_of_expr (TypedExpr async_typed_expr2))
            (string_of_expr (TypedExpr next_typed_expr)) )
  )


let eval_step thread_pool heap scheduled_thread_id = 
  get_thread scheduled_thread_id thread_pool >>=
  fun (TThread(_,expr, stack)) -> 
  step_expr expr stack heap thread_pool

  >>| fun (updated_expr,updated_stack, updated_heap, updated_thread_pool) ->  
  let new_thread = TThread(scheduled_thread_id, updated_expr, updated_stack) in 
  (replace_thread new_thread updated_thread_pool), updated_heap)
