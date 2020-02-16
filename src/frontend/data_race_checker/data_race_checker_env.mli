(** This module contains helper functions used in the data-race checking stage *)

open Core
open Ast.Ast_types

val get_class_regions :
  Class_name.t -> Desugaring.Desugared_ast.class_defn list -> region list Or_error.t

val get_class_field_regions :
     Class_name.t
  -> Field_name.t
  -> Desugaring.Desugared_ast.class_defn list
  -> region list Or_error.t

val elem_in_list : 'a -> 'a list -> bool
