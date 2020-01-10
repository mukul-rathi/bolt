open Core
open Ast.Ast_types
open Type_expr
open Type_region_annotations

let init_env_from_params params =
  List.map
    ~f:(function TParam (type_expr, param_name, _) -> (param_name, type_expr))
    params

let type_function_defn class_defns function_defns
    (Parsing.Parsed_ast.TFunction (func_name, return_type, params, body_expr)) =
  let open Result in
  type_params_region_annotations class_defns params
  >>= fun () ->
  infer_type_expr class_defns function_defns body_expr (init_env_from_params params)
  >>= fun (typed_body_expr, body_return_type) ->
  if body_return_type = return_type then
    Ok (Typed_ast.TFunction (func_name, return_type, params, typed_body_expr))
  else
    Error
      (Error.of_string
         (Fmt.str
            "Type Error for function %s: expected return type of %s but got %s instead"
            (Function_name.to_string func_name)
            (string_of_type return_type)
            (string_of_type body_return_type)))

let type_function_defns class_defns function_defns =
  Result.all (List.map ~f:(type_function_defn class_defns function_defns) function_defns)
