open Core
open Type_data_races_expr
open Desugaring.Desugared_ast

let type_data_races_method_defn class_defns
    (TMethod (method_name, ret_type, params, region_effects, body_expr)) =
  let open Result in
  type_data_races_block_expr class_defns body_expr
  >>| fun data_race_checked_body_expr ->
  TMethod (method_name, ret_type, params, region_effects, data_race_checked_body_expr)

let type_data_races_class_defn class_defns
    (TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  Result.all (List.map ~f:(type_data_races_method_defn class_defns) method_defns)
  >>| fun data_race_checked_method_defns ->
  TClass (class_name, regions, fields, data_race_checked_method_defns)
