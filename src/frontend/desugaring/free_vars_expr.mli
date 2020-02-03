open Ast.Ast_types

val free_vars_expr : Desugared_ast.expr -> Var_name.t list
(** Return a list of the free variables in an expr *)

val free_vars_block_expr : Desugared_ast.block_expr -> Var_name.t list
(** Return a list of the free variables in a block expr *)
