open Core
open Ast.Ast_types
open Parsing
open Type_expr
open Type_overloading
open Type_generics
open Type_inheritance
open Type_env

let init_env_from_params params =
  List.map
    ~f:(function TParam (type_expr, param_name, _, _) -> (param_name, type_expr))
    params

let type_function_type_sig class_defns func_name params return_type =
  let function_error_prefix =
    Fmt.str "Type error for function %s" (Function_name.to_string func_name) in
  let return_type_error_prefix = Fmt.str "%s return type" function_error_prefix in
  let open Result in
  check_type_valid return_type class_defns return_type_error_prefix
  >>= fun () ->
  Result.all_unit
    (List.map
       ~f:(fun (TParam (param_type, param_name, _, _)) ->
         let param_error_prefix =
           Fmt.str "%s param %s" function_error_prefix (Var_name.to_string param_name)
         in
         check_type_valid param_type class_defns param_error_prefix)
       params)

let type_function_defn class_defns function_defns
    ( Parsed_ast.TFunction
        (func_name, maybe_borrowed_ret_ref, return_type, params, body_expr) as
    curr_function_defn ) =
  let open Result in
  type_function_type_sig class_defns func_name params return_type
  >>= fun () ->
  type_generics_usage_function_defn curr_function_defn
  >>= fun () ->
  type_block_expr class_defns function_defns body_expr (init_env_from_params params)
  >>= fun (typed_body_expr, body_return_type) ->
  (* We throw away returned expr if return type is void *)
  if return_type = TEVoid || is_subtype_of class_defns body_return_type return_type then
    Ok
      (Typed_ast.TFunction
         (func_name, maybe_borrowed_ret_ref, return_type, params, typed_body_expr))
  else
    Error
      (Error.of_string
         (Fmt.str
            "Type Error for function %s: expected return type of %s but got %s instead"
            (Function_name.to_string func_name)
            (string_of_type return_type)
            (string_of_type body_return_type)))

let type_function_defns class_defns function_defns =
  let open Result in
  type_overloaded_function_defns function_defns
  >>= fun () ->
  Result.all (List.map ~f:(type_function_defn class_defns function_defns) function_defns)
