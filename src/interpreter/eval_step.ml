open Runtime_env
open Core
open Result

let remove_thread thread_id thread_pool =
  List.filter ~f:(fun (TThread (tid, _, _)) -> not (tid = thread_id)) thread_pool

let replace_thread (TThread (thread_id, instructions, stack)) thread_pool =
  TThread (thread_id, instructions, stack) :: remove_thread thread_id thread_pool

let get_thread thread_id thread_pool =
  List.filter ~f:(fun (TThread (tid, _, _)) -> not (tid = thread_id)) thread_pool
  |> function
  | []              ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: Thread with id: %s not present.@."
              (string_of_thread_id thread_id)))
  | [thread] -> Ok thread
  | _               ->
      Error
        (Error.of_string
           (Fmt.str "Runtime error: Duplicate threads with id: %s  present.@."
              (string_of_thread_id thread_id)))

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
        | V (THREAD_ID tid) :: _ -> Ok tid (* Read value of thread id from stack *)
        | _                           ->
            Error
              (Error.of_string
                 "Runtime error: Value not present on top of stack to bind to variable.")
        )
        >>= fun thread_id ->
        has_thread_completed thread_id thread_pool
        >>| function
        | true  -> (instrs, stack, heap, remove_thread thread_id thread_pool)
        (* thread we are waiting on has completed, so remove it from the thread_pool and
           continue executing *)
        | false -> (instr :: instrs, stack, heap, thread_pool)
        (* wait on execution of thread to finish *) )
    | MK_CLOSURE code              ->
        get_free_var_bindings code stack
        >>| fun env -> (instrs, V (CLOSURE (code, env)) :: stack, heap, thread_pool)
    | STACK_LOOKUP var_name        ->
        stack_lookup stack var_name >>| fun v -> (instrs, V v :: stack, heap, thread_pool)
    | STACK_SET var_name           -> (
      match stack with
      | V v :: stk ->
          stack_set_var stk var_name v
          >>| fun updated_stack -> (instrs, updated_stack, heap, thread_pool)
      | _               ->
          Error
            (Error.of_string
               "Runtime error: Value not present on top of stack to set to variable.") )
    | HEAP_FIELD_LOOKUP field_name -> (
      match stack with
      | V (ADDR addr) :: stk ->
          heap_lookup_field heap addr field_name
          >>| fun v -> (instrs, V v :: stk, heap, thread_pool)
      | _                         ->
          Error
            (Error.of_string
               "Runtime error: Address not present on top of stack to look up field in \
                heap.") )
    | HEAP_FIELD_SET field_name    -> (
      match stack with
      | V (ADDR addr) :: V v :: stk ->
          heap_set_field heap addr field_name v
          >>| fun updated_heap ->
          (instrs, V (ADDR addr) :: stk, updated_heap, thread_pool)
      (* Note we keep the address on top of the stack in case we use it for future fields
         set e.g when creating a new object *)
      | _                                       ->
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
            (Error.of_string "Runtime error: Can't swap elements if size of stack < .") )
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
        Ok (instrs, V (ADDR addr) :: stack, updated_heap, thread_pool)
    | SPAWN code                   ->
        get_free_var_bindings code stack
        >>| fun env ->
        spawn_thread thread_pool code [Env env]
        |> fun (thread_id, updated_thread_pool) ->
        (instrs, V (THREAD_ID thread_id) :: stack, heap, updated_thread_pool) )

let eval_step thread_pool heap scheduled_thread_id =
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
