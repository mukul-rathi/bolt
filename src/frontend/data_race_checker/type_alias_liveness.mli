(** This module checks the liveness of aliases for a object with linear capability , and
    updates updates the linear object reference to no longer have the linear capability
    when aliases are live.

    (Same concept as Non-Lexical-Lifetimes in Rust) *)

open Desugaring.Desugared_ast
open Ast.Ast_types

val type_alias_liveness_block_expr :
     Var_name.t
  -> Var_name.t list
  -> (region list -> region -> bool)
  -> Var_name.t list
  -> block_expr
  -> block_expr * Var_name.t list
(** Takes in as args the [Var_name.t] name of the aliased object, the list of possible
    aliases, a region filter function that filters out regions needing linear
    capabilities, a list of the current live aliases and the block expression.

    Returns the updated block expression along with the live variables at the start of the
    block. *)
