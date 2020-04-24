open Core
open Ast.Ast_types
open Type_data_races_expr
open Desugaring.Desugared_ast
open Type_capability_annotations
open Type_capability_constraints
open Data_race_checker_env
open Type_borrowing

let type_data_races_function_defn class_defns function_defns ~ignore_data_races
    (TFunction (func_name, maybe_borrowed_ref_ret, ret_type, params, body_expr)) =
  let open Result in
  type_params_capability_annotations class_defns params
  >>= fun () ->
  let error_prefix =
    Fmt.str "Potential data race in function %s " (Function_name.to_string func_name)
  in
  let param_obj_var_capabilities =
    params_to_obj_vars_and_capabilities class_defns params in
  type_function_reverse_borrowing class_defns error_prefix ret_type maybe_borrowed_ref_ret
    body_expr
  >>= fun () ->
  type_param_capability_constraints param_obj_var_capabilities body_expr
  |> fun param_constrained_body_expr ->
  type_data_races_block_expr class_defns function_defns param_constrained_body_expr
    param_obj_var_capabilities ~ignore_data_races
  >>| fun data_race_checked_body_expr ->
  TFunction
    (func_name, maybe_borrowed_ref_ret, ret_type, params, data_race_checked_body_expr)
