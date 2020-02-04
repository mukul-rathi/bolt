open Core

val type_regions_constraints_expr : Data_race_checker_ast.expr -> unit Or_error.t

val type_regions_constraints_block_expr :
  Data_race_checker_ast.block_expr -> unit Or_error.t
