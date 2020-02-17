(** This module contains helper functions used in the data-race checking stage *)

open Ast.Ast_types
open Desugaring.Desugared_ast

val elem_in_list : 'a -> 'a list -> bool
val class_has_capability : Class_name.t -> capability -> class_defn list -> bool
val type_has_capability : type_expr -> capability -> class_defn list -> bool

val region_fields_have_capability :
  region -> Class_name.t -> capability -> class_defn list -> bool

val get_class_regions : Class_name.t -> class_defn list -> region list

val get_class_field_regions :
  Class_name.t -> Field_name.t -> class_defn list -> region list

val get_class_region_fields :
  Class_name.t -> Region_name.t -> class_defn list -> field_defn list

val get_identifier_regions : identifier -> region list
val update_identifier_regions : identifier -> region list -> identifier

val params_to_obj_vars_and_regions :
  class_defn list -> param list -> obj_var_and_regions list
(** Convert a parameter to a representation which contains the regions it is allowed to
    access. *)

val can_concurrently_access_regions :
  Class_name.t -> class_defn list -> region -> region -> bool

val reduce_expr_to_obj_id : expr -> identifier list
(** Use abstract interpretation to return the potential object identifiers this expression
    reduces to, if it reduces to an identifier. *)

val reduce_block_expr_to_obj_id : block_expr -> identifier list
