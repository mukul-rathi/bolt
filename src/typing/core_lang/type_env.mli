(** The type environment module consists of the definition of a type environment and
    helper functions to operate on the environment *)

open Ast.Ast_types
open Core

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

val check_type_equality : type_expr -> type_expr -> bool

val field_to_expr_type : type_field -> type_expr
(**Converts a field type to the equivalent expression type - used to check type equality *)

(** A bunch of getter methods *)

val get_var_type : Var_name.t -> type_env -> loc -> type_expr Or_error.t
val get_class_field : Field_name.t -> class_defn -> loc -> field_defn Or_error.t

val get_obj_class_defn :
  Var_name.t -> type_env -> class_defn list -> loc -> class_defn Or_error.t

val get_class_defn : Class_name.t -> class_defn list -> loc -> class_defn Or_error.t
val get_type_capability : type_expr -> class_defn list -> loc -> capability Or_error.t

val get_function_type :
     Function_name.t
  -> Parsing.Parsed_ast.function_defn list
  -> loc
  -> (type_expr list * type_expr) Or_error.t

val get_function_body_expr :
  Function_name.t -> Typed_ast.function_defn list -> loc -> Typed_ast.expr Or_error.t
