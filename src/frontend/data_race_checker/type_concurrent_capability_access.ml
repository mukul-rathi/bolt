open Ast.Ast_types
open Core
open Data_race_checker_env

(* There is another capability in the class that can both access subordinate state in one
   capability and also subordinate state in another capability - thus acting as a channel
   for them. *)
let capabilities_have_subord_channel class_name class_defns capability_1_name
    capability_2_name =
  let get_cap_subord_fields capability_name =
    List.filter
      ~f:(fun (TField (_, field_type, _, _)) ->
        type_has_mode field_type Subordinate class_defns)
      (get_class_capability_fields class_name capability_name class_defns) in
  (* collect the capabilities that aren't capability 1 or capability 2 and have access to
     subord state*)
  let get_potential_channel_capabilities sub_ord_fields =
    List.concat_map
      ~f:(fun (TField (_, _, _, field_cap_names)) ->
        List.filter
          ~f:(fun cap_name ->
            (not (cap_name = capability_1_name)) && not (cap_name = capability_2_name))
          field_cap_names)
      sub_ord_fields in
  let get_capability_1_potential_channels =
    get_potential_channel_capabilities (get_cap_subord_fields capability_1_name) in
  let get_capability_2_potential_channels =
    get_potential_channel_capabilities (get_cap_subord_fields capability_1_name) in
  (* check if a capability in intersection of these potential channels *)
  let subord_channels =
    intersect_lists get_capability_1_potential_channels
      get_capability_2_potential_channels in
  List.length subord_channels > 0

(* Check that overlapping fields are not subordinate *)
let capabilities_have_no_subord_shared_state class_name class_defns capability_1_name
    capability_2_name =
  let capability_1_fields =
    get_class_capability_fields class_name capability_1_name class_defns in
  let capability_2_fields =
    get_class_capability_fields class_name capability_2_name class_defns in
  let shared_fields = intersect_lists capability_1_fields capability_2_fields in
  List.for_all
    ~f:(fun (TField (_, field_type, _, _)) ->
      not (type_has_mode field_type Subordinate class_defns))
    shared_fields

(* Check that overlapping fields are safe - i.e. either we're accessing them with a safe
   mode, or they themselves are safe *)
let capabilities_have_safe_shared_state class_name class_defns
    (TCapability (capability_1_mode, capability_1_name))
    (TCapability (capability_2_mode, capability_2_name)) =
  let capabilities_modes_are_safe capability_1_mode capability_2_mode =
    capability_mode_present capability_1_mode ThreadSafe
    && capability_mode_present capability_2_mode ThreadSafe in
  let capability_1_fields =
    get_class_capability_fields class_name capability_1_name class_defns in
  let capability_2_fields =
    get_class_capability_fields class_name capability_2_name class_defns in
  let shared_fields = intersect_lists capability_1_fields capability_2_fields in
  capabilities_modes_are_safe capability_1_mode capability_2_mode
  || List.for_all
       ~f:(fun (TField (_, field_type, _, _)) ->
         type_has_mode field_type ThreadSafe class_defns)
       shared_fields

let can_concurrently_access_capabilities class_name class_defns
    (TCapability (capability_1_mode, capability_1_name) as capability1)
    (TCapability (_, capability_2_name) as capability2) =
  capabilities_have_safe_shared_state class_name class_defns capability1 capability2
  && capabilities_have_no_subord_shared_state class_name class_defns capability_1_name
       capability_2_name
  && (not
        (capabilities_have_subord_channel class_name class_defns capability_1_name
           capability_2_name))
  (* Can't access the same linear capability in multiple threads as violates linearity *)
  && not (capability_1_mode = Linear && capability_1_name = capability_2_name)

(* We check that the capabilities can be accessed concurrently *)
let type_concurrent_capability_pair_constraints_var class_defns obj_class obj_name
    capabilities_thread1 capabilities_thread2 loc =
  Result.all_unit
    (List.map
       ~f:(fun (TCapability (_, capability_thread1_name) as capability_thread1) ->
         Result.all_unit
           (List.map
              ~f:(fun (TCapability (_, capability_thread2_name) as capability_thread2) ->
                if
                  can_concurrently_access_capabilities obj_class class_defns
                    capability_thread1 capability_thread2
                then Ok ()
                else
                  Error
                    (Error.of_string
                       (Fmt.str
                          "Potential data race: %s Can't access capabilities %s and %s of object %s concurrently@."
                          (string_of_loc loc)
                          (Capability_name.to_string capability_thread1_name)
                          (Capability_name.to_string capability_thread2_name)
                          (Var_name.to_string obj_name))))
              capabilities_thread2))
       capabilities_thread1)

let rec type_concurrent_capabilities_constraints_var class_defns obj_name obj_class
    all_threads_capabilities loc =
  match all_threads_capabilities with
  | [] -> Ok ()
  | thread_1_capabilities :: other_threads_capabilities ->
      let open Result in
      Result.all_unit
        (List.map
           ~f:(fun thread_2_capabilities ->
             type_concurrent_capability_pair_constraints_var class_defns obj_class
               obj_name thread_1_capabilities thread_2_capabilities loc)
           other_threads_capabilities)
      >>= fun () ->
      type_concurrent_capabilities_constraints_var class_defns obj_name obj_class
        other_threads_capabilities loc

let type_concurrent_capability_constraints_vars class_defns threads_free_vars loc =
  let var_names_and_classes =
    List.dedup_and_sort
      ~compare:(fun a b -> if a = b then 0 else 1)
      (List.map
         ~f:(fun (var_name, class_name, _) -> (var_name, class_name))
         threads_free_vars) in
  Result.all_unit
    (List.map (* check constraint for each object *)
       ~f:(fun (obj_name, obj_class) ->
         List.filter_map
           ~f:(fun (var_name, class_name, capabilities) ->
             if var_name = obj_name && class_name = obj_class then Some capabilities
             else None)
           threads_free_vars
         |> fun all_threads_obj_capabilities ->
         type_concurrent_capabilities_constraints_var class_defns obj_name obj_class
           all_threads_obj_capabilities loc)
       var_names_and_classes)
