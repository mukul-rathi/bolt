(** This module filters the regions available for an identifier to use based on
    constraints from previous data-race type-checking steps.

    E.g. if a reference doesn't have a linear capability, then it cannot use linear
    regions *)

val apply_regions_cap_constraints_expr :
  Data_race_checker_ast.expr -> Data_race_checker_ast.expr

val apply_regions_cap_constraints_block_expr :
  Data_race_checker_ast.block_expr -> Data_race_checker_ast.block_expr
