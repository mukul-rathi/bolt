open Core

(** This module takes in a desugared AST and renames variable names to ensure there is no
    variable shadowing *)

val remove_var_shadowing_program :
  Desugared_ast.program -> Desugared_ast.program Or_error.t
