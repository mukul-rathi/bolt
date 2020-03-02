(** This module contains helper functions used in the data-race checking stage *)

open Ast.Ast_types
open Desugaring.Desugared_ast

val elem_in_list : 'a -> 'a list -> bool
val is_subset_of : 'a list -> 'a list -> bool
val var_lists_are_equal : Var_name.t list -> Var_name.t list -> bool
val identifier_matches_var_name : Var_name.t -> identifier -> bool
val class_has_mode : Class_name.t -> mode -> class_defn list -> bool
val type_has_mode : type_expr -> mode -> class_defn list -> bool
val identifier_has_mode : identifier -> mode -> class_defn list -> bool

val capability_fields_have_mode :
  capability -> Class_name.t -> mode -> class_defn list -> bool

val get_class_capabilities : Class_name.t -> class_defn list -> capability list

val get_class_field_capabilities :
  Class_name.t -> Field_name.t -> class_defn list -> capability list

val get_class_capability_fields :
  Class_name.t -> Capability_name.t -> class_defn list -> field_defn list

val get_identifier_name : identifier -> Var_name.t
val get_identifier_capabilities : identifier -> capability list
val set_identifier_capabilities : identifier -> capability list -> identifier
val get_function_params : Function_name.t -> function_defn list -> param list
val get_method_params : Class_name.t -> Method_name.t -> class_defn list -> param list

val param_to_obj_var_and_capabilities :
  class_defn list -> param -> obj_var_and_capabilities option

val params_to_obj_vars_and_capabilities :
  class_defn list -> param list -> obj_var_and_capabilities list
(** Convert a parameter to a representation which contains the capabilities it is allowed
    to access. *)

val get_method_capabilities_used :
  Class_name.t -> Method_name.t -> class_defn list -> capability list

val can_concurrently_access_capabilities :
  Class_name.t -> class_defn list -> capability -> capability -> bool

val reduce_expr_to_obj_id : expr -> identifier list
(** Use abstract interpretation to return the potential object identifiers this expression
    reduces to, if it reduces to an identifier. *)

val reduce_block_expr_to_obj_id : block_expr -> identifier list
