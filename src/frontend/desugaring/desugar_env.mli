(** This module contains helper functions used in the desugaring stage *)

open Ast.Ast_types

val get_class_capabilities :
  Class_name.t -> Typing.Typed_ast.class_defn list -> capability list

val get_class_field_capabilities :
  Class_name.t -> Field_name.t -> Typing.Typed_ast.class_defn list -> capability list

val elem_in_list : 'a -> 'a list -> bool

val name_mangle_method : Method_name.t -> type_expr list -> Method_name.t
(** Name mangle method names to distinguish between overloaded methods *)

val name_mangle_function : Function_name.t -> type_expr list -> Function_name.t
