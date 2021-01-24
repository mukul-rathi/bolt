open Ast.Ast_types
open Parsing
open Core
open Type_env
open Type_inheritance

let string_of_args_types = function
  | []              -> string_of_type TEVoid
  | _ as args_types -> String.concat ~sep:" * " (List.map ~f:string_of_type args_types)

(* Check overloaded function and method definitions *)

let type_overloaded_params error_prefix params_list =
  if
    not
      (List.contains_dup
         ~compare:(fun params1 params2 ->
           List.compare
             (fun (TParam (type_1, _, _, _)) (TParam (type_2, _, _, _)) ->
               if type_1 = type_2 then 0 else 1)
             params1 params2)
         params_list)
  then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "%s has duplicate definitions in environment@." error_prefix))

let type_overloaded_func_name func_name function_defns =
  let overloaded_function_params =
    List.filter_map
      ~f:(fun (Parsed_ast.TFunction (name, _, _, params, _)) ->
        if func_name = name then Some params else None)
      function_defns in
  let error_prefix =
    Fmt.str "Type error - function %s" (Function_name.to_string func_name) in
  type_overloaded_params error_prefix overloaded_function_params

let type_overloaded_function_defns function_defns =
  let distinct_func_names =
    List.map
      ~f:(fun (Parsed_ast.TFunction (func_name, _, _, _, _)) -> func_name)
      function_defns in
  Result.all_unit
    (List.map
       ~f:(fun func_name -> type_overloaded_func_name func_name function_defns)
       distinct_func_names)

let type_overloaded_method_name method_name function_defns =
  let overloaded_method_params =
    List.filter_map
      ~f:(fun (Parsed_ast.TMethod (name, _, _, params, _, _)) ->
        if method_name = name then Some params else None)
      function_defns in
  let error_prefix =
    Fmt.str "Type error - method %s" (Method_name.to_string method_name) in
  type_overloaded_params error_prefix overloaded_method_params

let type_overloaded_method_defns method_defns =
  let distinct_method_names =
    List.map
      ~f:(fun (Parsed_ast.TMethod (method_name, _, _, _, _, _)) -> method_name)
      method_defns in
  Result.all_unit
    (List.map
       ~f:(fun method_name -> type_overloaded_method_name method_name method_defns)
       distinct_method_names)

(* Functions might be overloaded, and thus have multiple params+return types. Use the arg
   types to pick the right overloaded function call: return the matching param and return
   types.*)
let get_matching_params_and_ret_type class_defns error_prefix params_and_ret_types
    args_types =
  match params_and_ret_types with
  | [] ->
      (* no function defined *)
      Error (Error.of_string (Fmt.str "%s is not defined in environment@." error_prefix))
  | [(param_types, return_type)] (* we have a single non-overloaded function *) ->
      if are_subtypes_of class_defns args_types param_types then
        Ok (param_types, return_type)
      else
        Error
          (Error.of_string
             (Fmt.str "%s expected arguments of type %s, instead received type %s@."
                error_prefix
                (string_of_args_types param_types)
                (string_of_args_types args_types)))
  | _ -> (
      (* We have function overloading so pick most specific function. Try to choose the
         function whose param types match the argument types. If not, then pick the
         function where the args are subtypes of the params. If there are multiple
         functions that fit the bill, raise an error. *)
      List.find ~f:(fun (param_types, _) -> args_types = param_types) params_and_ret_types
      |> function
      | Some params_and_ret_type -> Ok params_and_ret_type
      | None                     -> (
          List.filter
            ~f:(fun (param_types, _) ->
              are_subtypes_of class_defns args_types param_types)
            params_and_ret_types
          |> function
          | []                     ->
              Error
                (Error.of_string
                   (Fmt.str "%s has no matching definition that accepts args of type %s@."
                      error_prefix
                      (string_of_args_types args_types)))
          | [params_and_ret_types] -> Ok params_and_ret_types
          | _                      ->
              Error
                (Error.of_string
                   (Fmt.str
                      "%s has multiple matching definition that accepts args of type %s@."
                      error_prefix
                      (string_of_args_types args_types))) ) )

let get_matching_function_type class_defns func_name args_types function_defns loc =
  let overloaded_function_param_and_ret_types =
    List.filter_map
      ~f:(fun (Parsed_ast.TFunction (name, _, return_type, params, _)) ->
        if func_name = name then Some (get_params_types params, return_type) else None)
      function_defns in
  let error_prefix =
    Fmt.str "%s Type error - function %s" (string_of_loc loc)
      (Function_name.to_string func_name) in
  get_matching_params_and_ret_type class_defns error_prefix
    overloaded_function_param_and_ret_types args_types

let get_matching_method_type class_defns method_name args_types curr_class_defn
    maybe_type_param loc =
  let open Result in
  get_class_methods class_defns curr_class_defn maybe_type_param loc
  >>= fun method_defns ->
  List.filter_map
    ~f:(fun (Parsed_ast.TMethod (name, _, return_type, params, _, _)) ->
      if method_name = name then Some (get_params_types params, return_type) else None)
    method_defns
  |> fun overloaded_method_param_and_ret_types ->
  let error_prefix =
    Fmt.str "%s Type error - method %s" (string_of_loc loc)
      (Method_name.to_string method_name) in
  get_matching_params_and_ret_type class_defns error_prefix
    overloaded_method_param_and_ret_types args_types
