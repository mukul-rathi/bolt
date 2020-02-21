(** This module collates all region accesses for objects in a given thread, and, if an
    identifier has multiple region options, we choose which region to use and update the
    identifier accordingly. *)

open Desugaring.Desugared_ast

val collate_region_accesses_expr :
  class_defn list -> function_defn list -> expr -> expr * obj_var_and_regions list

val collate_region_accesses_block_expr :
     class_defn list
  -> function_defn list
  -> block_expr
  -> block_expr * obj_var_and_regions list
