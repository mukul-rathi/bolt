(** This module checks that any region annotations to function/method are valid regions *)

open Core
open Ast.Ast_types
open Parsing.Parsed_ast

val type_params_region_annotations : class_defn list -> param list -> unit Or_error.t

val type_method_effect_region_annotations :
  Class_name.t -> region list -> Region_name.t list -> unit Or_error.t
