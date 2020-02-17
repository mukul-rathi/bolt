(** This module type-checks potential accesses to thread regions of objects *)

open Desugaring.Desugared_ast

val type_thread_regions_expr : class_defn list -> expr -> expr
val type_thread_regions_block_expr : class_defn list -> block_expr -> block_expr
