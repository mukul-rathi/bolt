(** This module desugars the typed AST's exprs *)

open Ast.Ast_types
open Typing

val desugar_expr :
     Typed_ast.class_defn list
  -> Typed_ast.function_defn list
  -> Var_name.t list (** list of borrowed variables *)
  -> Typed_ast.expr
  -> Desugared_ast.expr

val desugar_block_expr :
     Typed_ast.class_defn list
  -> Typed_ast.function_defn list
  -> Var_name.t list (** list of borrowed variables *)
  -> Typed_ast.block_expr
  -> Desugared_ast.block_expr
