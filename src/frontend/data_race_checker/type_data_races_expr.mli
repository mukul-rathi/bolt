(** This module checks a expression for potential data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_block_expr :
     class_defn list
  -> function_defn list
  -> ignore_data_races:bool
  -> block_expr
  -> obj_var_and_capabilities list
  -> block_expr Or_error.t
(** If ignore_data_races flag set, will check capabilities but won't enforce constraints. *)
