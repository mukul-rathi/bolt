open Core
open Type_consume_expr
open Type_region_constraints
open Type_read_regions
open Type_async_regions
open Type_subord_regions
open Type_linear_regions
open Type_function_borrowing

let type_data_races_block_expr class_defns function_defns block_expr obj_vars_and_regions
    =
  let open Result in
  type_read_regions_block_expr block_expr
  |> type_subord_regions_block_expr class_defns obj_vars_and_regions
  |> type_async_regions_block_expr class_defns
  |> type_linear_regions_block_expr class_defns
  >>= fun typed_block_expr ->
  Result.ignore_m (type_consume_block_expr class_defns typed_block_expr [])
  >>= fun () ->
  type_function_forward_borrowing_block_expr class_defns function_defns typed_block_expr
  >>= fun () ->
  type_regions_constraints_block_expr class_defns function_defns typed_block_expr
  >>| fun () -> typed_block_expr
