open Core
open Type_consume_expr
open Type_region_constraints

let type_data_races_block_expr class_defns block_expr =
  let open Result in
  Result.ignore_m (type_consume_block_expr class_defns block_expr [])
  >>= fun () -> type_regions_constraints_block_expr block_expr >>| fun () -> block_expr
