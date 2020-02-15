open Core
open Type_data_races_expr
open Desugaring.Desugared_ast

let type_data_races_function_defn class_defns
    (TFunction (func_name, ret_type, params, body_expr)) =
  let open Result in
  type_data_races_block_expr class_defns body_expr
  >>| fun data_race_checked_body_expr ->
  TFunction (func_name, ret_type, params, data_race_checked_body_expr)
