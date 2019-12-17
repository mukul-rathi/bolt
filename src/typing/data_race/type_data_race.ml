open Core
open Type_linear_ownership
open Type_async_expr
open Result

let type_data_race program =
  type_program_async_exprs program >>= fun () -> type_linear_ownership program
