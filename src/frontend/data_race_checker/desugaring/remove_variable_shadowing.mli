open Core
open Ast.Ast_types

(** This module takes in a desugared AST and renames variable names to ensure there is no
    variable shadowing *)

type var_name_map = (Var_name.t * Var_name.t) list

val remove_var_shadowing_program :
  Data_race_checker_ast.program -> Data_race_checker_ast.program Or_error.t

val remove_var_shadowing_expr :
     Data_race_checker_ast.expr
  -> var_name_map
  -> (Data_race_checker_ast.expr * var_name_map) Or_error.t
