open Runtime_env
open Core
open Result

let has_thread_completed thread_id thread_pool =
  get_thread thread_id thread_pool
  >>| fun (TThread (_, instr_list, _)) -> match instr_list with [] -> true | _ -> false

let execute_instruction code stack heap thread_pool =
  match code with
  | []                 -> Error
                            (Error.of_string "Runtime error: No instructions to execute.")
  | instr :: instrs -> (
    match instr with
    | PUSH v                       -> Ok (instrs, V v :: stack, heap, thread_pool)
    | BIND var_name                -> (
      match stack with
      | V v :: stk -> Ok (instrs, Env [(var_name, v)] :: stk, heap, thread_pool)
      | _               ->
          Error
            (Error.of_string
               "Runtime error: Value not present on top of stack to bind to variable.") )
    | BLOCKED                      -> (
        ( match stack with
        | V (THREAD_ID tid) :: stk -> Ok (tid, stk)
        (* Read value of thread id from stack to determine which thread we're waiting on *)
        | _                             ->
            Error
              (Error.of_string
                 "Runtime error: Value not present on top of stack to bind to variable.")
        )
        >>= fun (thread_id, stk) ->
        has_thread_completed thread_id thread_pool
        >>| function
        | true  -> (instrs, stk, heap, remove_thread thread_id thread_pool)
        (* thread we are waiting on has completed, so remove it from the thread_pool, pop
           off the thread id from stack and continue executing *)
        | false -> (instr :: instrs, stack, heap, thread_pool)
        (* wait on execution of thread to finish *) )
    | MK_CLOSURE code              ->
        get_free_var_bindings code stack
        |> fun env -> Ok (instrs, V (CLOSURE (code, env)) :: stack, heap, thread_pool)
    | STACK_LOOKUP var_name        ->
        stack_lookup stack var_name >>| fun v -> (instrs, V v :: stack, heap, thread_pool)
    | STACK_SET var_name           -> (
      match stack with
      | V v :: stk ->
          stack_set_var stk var_name v
          |> fun updated_stack -> Ok (instrs, updated_stack, heap, thread_pool)
      | _               ->
          Error
            (Error.of_string
               "Runtime error: Value not present on top of stack to set to variable.") )
    | HEAP_FIELD_LOOKUP field_name -> (
      match stack with
      | V (REF addr) :: stk ->
          heap_lookup_field heap addr field_name
          >>| fun v -> (instrs, V v :: stk, heap, thread_pool)
      | _                        ->
          Error
            (Error.of_string
               "Runtime error: Address not present on top of stack to look up field in \
                heap.") )
    | HEAP_FIELD_SET field_name    -> (
      match stack with
      | V (REF addr) :: V v :: stk ->
          heap_set_field heap addr field_name v
          >>| fun updated_heap -> (instrs, V (REF addr) :: stk, updated_heap, thread_pool)
      (* Note we keep the address on top of the stack in case we use it for future fields
         set e.g when creating a new object *)
      | _                                      ->
          Error
            (Error.of_string
               "Runtime error: Address and value not present on top of stack to set \
                field in heap.") )
    | SWAP                         -> (
      match stack with
      | stk_item1 :: stk_item2 :: stk ->
          Ok (instrs, stk_item2 :: stk_item1 :: stk, heap, thread_pool)
      | _                                     ->
          Error
            (Error.of_string "Runtime error: Can't swap elements if size of stack < 2.")
      )
    | POP                          -> (
      match stack with
      | _ :: stk -> Ok (instrs, stk, heap, thread_pool)
      | _           -> Error
                         (Error.of_string
                            "Runtime error: Can't pop off item from empty stack") )
    | APPLY                        -> (
      match stack with
      | V arg :: V (CLOSURE (code, env)) :: stk ->
          Ok (code @ instrs, V arg :: Env env :: stk, heap, thread_pool)
      | _                                                   -> Error
                                                                 (Error.of_string
                                                                    "Runtime error: \
                                                                     Can't pop off item \
                                                                     from empty stack") )
    | CONSTRUCTOR class_name       ->
        create_obj heap class_name
        |> fun (addr, updated_heap) ->
        Ok (instrs, V (REF addr) :: stack, updated_heap, thread_pool)
    | SPAWN code                   ->
        (* Create a new thread with the instructions specified *)
        get_free_var_bindings code stack
        |> fun env ->
        spawn_thread thread_pool code [Env env]
        (* new thread inherits the most recent variable bindings on stack *)
        |> fun (thread_id, updated_thread_pool) ->
        (* Push thread id on stack, (used by BLOCKED to determine which thread we are
           waiting on)*)
        Ok (instrs, V (THREAD_ID thread_id) :: stack, heap, updated_thread_pool) )

let eval_step thread_pool heap ~scheduled_thread_id =
  get_thread scheduled_thread_id thread_pool
  >>= fun (TThread (_, instrs, stack)) ->
  match instrs with
  | [] -> Ok (thread_pool, heap) (* No instructions to execute *)
  | _  ->
      (* We have instructions to execute *)
      execute_instruction instrs stack heap thread_pool
      >>| fun (updated_instrs, updated_stack, updated_heap, updated_thread_pool) ->
      let new_thread = TThread (scheduled_thread_id, updated_instrs, updated_stack) in
      (replace_thread new_thread updated_thread_pool, updated_heap)
