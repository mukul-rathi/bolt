open Core
open Desugaring.Desugared_ast

val type_param_capability_constraints :
  obj_var_and_capabilities list -> block_expr -> block_expr
(** Enforce the constraints placed function params and method effects. *)

val type_capabilities_constraints_expr :
  class_defn list -> function_defn list -> expr -> unit Or_error.t

val type_capabilities_constraints_block_expr :
  class_defn list -> function_defn list -> block_expr -> unit Or_error.t
