open Core
open Type_consume_expr
open Type_capability_constraints
open Type_read_capabilities
open Type_async_capabilities
open Type_subord_capabilities
open Type_linear_capabilities
open Type_function_borrowing
open Aggregate_capability_accesses

let type_data_races_block_expr class_defns function_defns ~ignore_data_races block_expr
    obj_vars_and_capabilities =
  let open Result in
  type_read_capabilities_block_expr block_expr
  |> type_subord_capabilities_block_expr class_defns obj_vars_and_capabilities
  |> type_async_capabilities_block_expr class_defns
  |> type_linear_capabilities_block_expr class_defns
  >>= fun typed_block_expr ->
  Result.ignore_m (type_consume_block_expr class_defns typed_block_expr [])
  >>= fun () ->
  type_function_forward_borrowing_block_expr class_defns function_defns typed_block_expr
  >>= fun () ->
  aggregate_capability_accesses_block_expr class_defns function_defns typed_block_expr
  |> fun (capability_access_aggregated_block_expr, _) ->
  ( if ignore_data_races then Ok ()
  else
    type_capabilities_constraints_block_expr class_defns function_defns
      capability_access_aggregated_block_expr )
  >>| fun () -> capability_access_aggregated_block_expr
