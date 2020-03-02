(** This module checks that any capability annotations to function/method are valid
    capabilities *)

open Core
open Ast.Ast_types
open Desugaring.Desugared_ast

val type_params_capability_annotations : class_defn list -> param list -> unit Or_error.t
(** This checks if the guards specified in function/method parameters are actually valid
    capabilities *)

val type_field_capability_annotations :
  Class_name.t -> capability list -> Capability_name.t list -> capability list Or_error.t
(** This checks if the capabilities specified in fields are actually valid capabilities of
    the class *)
