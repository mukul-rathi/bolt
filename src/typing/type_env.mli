open Ast_types
open Core

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

val check_type_equality : type_expr -> type_expr -> bool
val field_to_expr_type : type_field -> type_expr
val get_var_type : Var_name.t -> type_env -> loc -> (type_expr, Error.t) result

val get_class_defn :
  Class_name.t -> class_defn list -> loc -> (class_defn, Error.t) result

val get_field_type : Field_name.t -> class_defn -> loc -> (type_field, Error.t) result
