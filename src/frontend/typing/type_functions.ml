open Core
open Ast.Ast_types
open Type_expr
open Type_overloading

let init_env_from_params params =
  List.map
    ~f:(function TParam (type_expr, param_name, _, _) -> (param_name, type_expr))
    params

let type_function_defn class_defns function_defns
    (Parsing.Parsed_ast.TFunction
      (func_name, maybe_borrowed_ret_ref, return_type, params, body_expr)) =
  let open Result in
  type_block_expr class_defns function_defns body_expr (init_env_from_params params)
  >>= fun (typed_body_expr, body_return_type) ->
  (* We throw away returned expr if return type is void *)
  if return_type = TEVoid || body_return_type = return_type then
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
