open Ast.Ast_types
open Desugaring.Desugared_ast

(** Update identifiers that match the given names, filtering by
    [capability list -> capability -> bool] - a function that given a list of all the
    capabilities, decides whether a capability should be in the identifier. *)

val update_matching_identifier_caps_block_expr :
  Var_name.t list -> (capability list -> capability -> bool) -> block_expr -> block_expr
