(** This module collates all capability accesses for objects in a given thread, and, if an
    identifier has multiple capability options, we choose which capability to use and
    update the identifier accordingly. *)

open Desugaring.Desugared_ast

val collate_capability_accesses_expr :
  class_defn list -> function_defn list -> expr -> expr * obj_var_and_capabilities list

val collate_capability_accesses_block_expr :
     class_defn list
  -> function_defn list
  -> block_expr
  -> block_expr * obj_var_and_capabilities list
