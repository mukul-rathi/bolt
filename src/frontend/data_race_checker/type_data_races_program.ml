open Core
open Type_data_races_expr
open Type_data_races_classes
open Type_data_races_functions
open Desugaring.Desugared_ast

let type_data_races_program (Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  Result.all
    (List.map ~f:(type_data_races_class_defn class_defns function_defns) class_defns)
  >>= fun data_race_checked_class_defns ->
  Result.all
    (List.map
       ~f:(type_data_races_function_defn class_defns function_defns)
       function_defns)
  >>= fun data_race_checked_function_defns ->
  type_data_races_block_expr class_defns function_defns main_expr []
  >>| fun data_race_checked_main_expr ->
  Prog
    ( data_race_checked_class_defns
    , data_race_checked_function_defns
    , data_race_checked_main_expr )

let pprint_data_race_checker_ast ppf prog = Desugaring.Pprint_dast.pprint_program ppf prog
