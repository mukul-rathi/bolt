open Core
open Desugaring

val type_regions_constraints_expr : Desugared_ast.expr -> unit Or_error.t
val type_regions_constraints_block_expr : Desugared_ast.block_expr -> unit Or_error.t
