(** This module contains helper functions used in the desugaring stage *)

open Ast.Ast_types

val get_class_regions : Class_name.t -> Typing.Typed_ast.class_defn list -> region list

val get_class_field_regions :
  Class_name.t -> Field_name.t -> Typing.Typed_ast.class_defn list -> region list

val elem_in_list : 'a -> 'a list -> bool
