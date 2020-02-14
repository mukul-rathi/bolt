open Core
open Remove_variable_shadowing
open Desugar_expr
open Type_consume_expr
open Type_region_constraints

let type_data_races_block_expr class_defns block_expr =
  let open Result in
  desugar_block_expr class_defns block_expr
  >>= fun desugared_block_expr ->
  Result.ignore_m (type_consume_block_expr desugared_block_expr [])
  >>= fun () ->
  type_regions_constraints_block_expr desugared_block_expr
  >>| fun () -> desugared_block_expr

let type_data_races_function_defn class_defns
    (Typing.Typed_ast.TFunction (func_name, ret_type, params, body_expr)) =
  let open Result in
  type_data_races_block_expr class_defns body_expr
  >>| fun data_race_checked_body_expr ->
  Data_race_checker_ast.TFunction
    (func_name, ret_type, params, data_race_checked_body_expr)

let type_data_races_method_defn class_defns
    (Typing.Typed_ast.TMethod (method_name, ret_type, params, region_effects, body_expr))
    =
  let open Result in
  type_data_races_block_expr class_defns body_expr
  >>| fun data_race_checked_body_expr ->
  Data_race_checker_ast.TMethod
    (method_name, ret_type, params, region_effects, data_race_checked_body_expr)

let type_data_races_class_defn class_defns
    (Typing.Typed_ast.TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  Result.all (List.map ~f:(type_data_races_method_defn class_defns) method_defns)
  >>| fun data_race_checked_method_defns ->
  Data_race_checker_ast.TClass
    (class_name, regions, fields, data_race_checked_method_defns)

let type_data_races_program
    (Typing.Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all (List.map ~f:(type_data_races_class_defn class_defns) class_defns)
  >>= fun data_race_checked_class_defns ->
  Result.all (List.map ~f:(type_data_races_function_defn class_defns) function_defns)
  >>= fun data_race_checked_function_defns ->
  type_data_races_block_expr class_defns main_expr
  >>= fun data_race_checked_main_expr ->
  remove_var_shadowing_program
    (Data_race_checker_ast.Prog
       ( data_race_checked_class_defns
       , data_race_checked_function_defns
       , data_race_checked_main_expr ))

let pprint_data_race_checker_ast ppf (prog : Data_race_checker_ast.program) =
  Pprint_dast.pprint_program ppf prog
