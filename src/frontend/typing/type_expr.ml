open Parsing
open Core
open Remove_variable_shadowing
open Infer_type_expr

let rec init_var_map_from_env = function
  | []                -> []
  | (param, _) :: env -> (param, param) :: init_var_map_from_env env

(* Top level statement to infer type of overall program expr *)
let type_expr class_defns function_defns (expr : Parsed_ast.expr) env =
  let open Result in
  infer_type_expr class_defns function_defns (expr : Parsed_ast.expr) env
  >>= fun (typed_expr, expr_type) ->
  let var_name_map = init_var_map_from_env env in
  remove_var_shadowing typed_expr var_name_map
  >>| fun (deshadowed_expr, _) -> (deshadowed_expr, expr_type)
