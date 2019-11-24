open Core
open Type_linear_ownership
open Type_async_expr
open Result
open Typed_ast

let type_data_race (Prog (class_defns, trait_defns, expr)) =
  (* Ignore results as we're only interested in whether there are type checking errors *)
  Result.ignore (type_async_expr class_defns trait_defns expr)
  >>= fun () -> Result.ignore (type_linear_ownership class_defns trait_defns expr)
