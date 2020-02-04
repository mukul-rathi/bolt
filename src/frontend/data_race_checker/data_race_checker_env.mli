(** This module contains helper functions used in the data-race checking stage *)

open Core
open Ast.Ast_types

val get_class_regions :
  Class_name.t -> Typing.Typed_ast.class_defn list -> region list Or_error.t

val get_class_field_regions :
     Class_name.t
  -> Field_name.t
  -> Typing.Typed_ast.class_defn list
  -> region list Or_error.t

(** Update identifier capabilities - for a given variable name x, this would update x, x.f
    x.g etc. Note: SIDE-EFFECTING *)

val update_var_capabilities_expr :
     Var_name.t
  -> (Data_race_checker_ast.capabilities -> unit)
  -> Data_race_checker_ast.expr
  -> unit

val update_var_capabilities_block_expr :
     Var_name.t
  -> (Data_race_checker_ast.capabilities -> unit)
  -> Data_race_checker_ast.block_expr
  -> unit
