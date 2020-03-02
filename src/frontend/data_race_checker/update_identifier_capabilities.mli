open Ast.Ast_types
open Desugaring.Desugared_ast

val find_aliases_in_block_expr :
  Var_name.t -> Var_name.t list -> block_expr -> Var_name.t list

val update_identifier_capabilities :
  Var_name.t -> (capability list -> capability -> bool) -> identifier -> identifier

val update_identifier_capabilities_expr :
  Var_name.t -> (capability list -> capability -> bool) -> expr -> expr
(** Update all identifiers that match the given name,filtering by
    [capability list -> capability -> bool] - a function that given a list of all the
    capabilities, decides whether a capability should be in the identifier. We also
    propagate the update to any aliases made of the identifier. (note this is not a
    symmetric propagation) *)

val update_identifier_capabilities_block_expr :
  Var_name.t -> (capability list -> capability -> bool) -> block_expr -> block_expr
