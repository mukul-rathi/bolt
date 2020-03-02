(** This module type-checks potential accesses to read capabilities of objects *)

open Desugaring.Desugared_ast

val type_read_capabilities_expr : expr -> expr
val type_read_capabilities_block_expr : block_expr -> block_expr
