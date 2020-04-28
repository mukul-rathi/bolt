(** This module instantiates generic class definitions once for each concrete type
    parameter *)

open Ast.Ast_types
open Typing

val instantiate_generic_class_defns :
     Typed_ast.class_defn list
  -> (Class_name.t * type_expr list) list
  -> Typed_ast.class_defn list
