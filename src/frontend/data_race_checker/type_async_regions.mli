(** This module type-checks potential accesses to regions of objects in async expressions *)

open Desugaring.Desugared_ast

val type_async_regions_expr : class_defn list -> expr -> expr
val type_async_regions_block_expr : class_defn list -> block_expr -> block_expr
