open Core
open Type_consume_expr
open Type_region_constraints
open Type_read_regions
open Type_thread_regions

let type_data_races_block_expr class_defns block_expr =
  let open Result in
  type_read_regions_block_expr block_expr
  |> type_thread_regions_block_expr class_defns
  |> fun typed_block_expr ->
  Result.ignore_m (type_consume_block_expr class_defns typed_block_expr [])
  >>= fun () ->
  type_regions_constraints_block_expr typed_block_expr >>| fun () -> typed_block_expr
