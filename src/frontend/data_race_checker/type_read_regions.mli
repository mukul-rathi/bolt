(** This module type-checks potential accesses to read regions of objects *)

open Desugaring.Desugared_ast

val type_read_regions_expr : expr -> expr
val type_read_regions_block_expr : block_expr -> block_expr
