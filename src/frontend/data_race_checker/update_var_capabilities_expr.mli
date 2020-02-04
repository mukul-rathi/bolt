(** Update identifier capabilities - for a given variable name x, this would update x, x.f
    x.g etc. and returns the updated expr *)

open Ast.Ast_types

val update_var_capabilities_expr :
     Var_name.t
  -> (Data_race_checker_ast.capabilities -> unit)
  -> Data_race_checker_ast.expr
  -> Data_race_checker_ast.expr

val update_var_capabilities_block_expr :
     Var_name.t
  -> (Data_race_checker_ast.capabilities -> unit)
  -> Data_race_checker_ast.block_expr
  -> Data_race_checker_ast.block_expr
