open Ast.Ast_types
open Desugaring.Desugared_ast

val update_identifier_regions_expr :
  Var_name.t -> (region list -> region -> bool) -> expr -> expr
(** Update all identifiers that match the given name,filtering by
    [region list -> region -> bool] - a function that given a list of all the regions,
    decides whether a region should be in the identifier. We also propagate the update to
    any aliases made of the identifier. (note this is not a symmetric propagation) *)

val update_identifier_regions_block_expr :
  Var_name.t -> (region list -> region -> bool) -> block_expr -> block_expr

val find_aliases_in_block_expr : Var_name.t -> block_expr -> Var_name.t list
