(** This module contains helper functions used in the desugaring stage *)

open Ast.Ast_types

val get_class_method_defns :
  Class_name.t -> Typing.Typed_ast.class_defn list -> Typing.Typed_ast.method_defn list

val get_class_capabilities :
  Class_name.t -> Typing.Typed_ast.class_defn list -> capability list

val get_class_field_capabilities :
  Class_name.t -> Field_name.t -> Typing.Typed_ast.class_defn list -> capability list

val maybe_get_superclass :
  Class_name.t -> Typing.Typed_ast.class_defn list -> Class_name.t option

val elem_in_list : 'a -> 'a list -> bool
