(** This module contains helper functions used in the data-race checking stage *)

open Core
open Ast.Ast_types
open Desugaring.Desugared_ast

val get_class_regions : Class_name.t -> class_defn list -> region list Or_error.t

val get_class_field_regions :
  Class_name.t -> Field_name.t -> class_defn list -> region list Or_error.t

val elem_in_list : 'a -> 'a list -> bool

val reduce_expr_to_obj_id : expr -> identifier list
(** Use abstract interpretation to return the potential object identifiers this expression
    reduces to, if it reduces to an identifier. *)

val reduce_block_expr_to_obj_id : block_expr -> identifier list
