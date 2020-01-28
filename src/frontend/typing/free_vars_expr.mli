open Ast.Ast_types

val free_vars_expr : Typed_ast.expr -> Var_name.t list
(** Return a list of the free variables in an expr *)
