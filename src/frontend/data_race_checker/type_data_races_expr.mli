(** This module checks a expression for potential data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_block_expr :
  class_defn list -> block_expr -> obj_var_and_regions list -> block_expr Or_error.t
