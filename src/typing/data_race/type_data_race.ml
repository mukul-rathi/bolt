open Core
open Type_linear_ownership
open Type_async_expr
open Result
open Typing_core_lang.Typed_ast

let type_data_race (Prog (class_defns, trait_defns, expr)) =
  type_async_expr class_defns trait_defns expr
  >>= fun () -> type_linear_ownership class_defns trait_defns expr
