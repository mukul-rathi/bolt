open Core
open Ast.Ast_types

(** This module takes in a typed AST and renames variable names to ensure there is no
    variable shadowing *)

type var_name_map = (Var_name.t * Var_name.t) list
(** Maps the old variable names to the new unique variable names *)

val remove_var_shadowing :
     Typing.Typed_ast.expr
  -> var_name_map
  -> (Typing.Typed_ast.expr * var_name_map) Or_error.t
(** Takes in a var_map and renames variables in the expr, returning the modified expr and
    the updated mapping from old -> new var names *)
