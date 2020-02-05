(** This module checks that any region annotations to function/method are valid regions *)

open Core
open Ast.Ast_types
open Parsing.Parsed_ast

val type_params_region_annotations : class_defn list -> param list -> unit Or_error.t
(** This checks if the guards specified in function/method parameters are actually valid
    regions *)

val type_intra_class_region_annotations :
  Class_name.t -> region list -> Region_name.t list -> unit Or_error.t
(** This checks if the effects specified in method annotation are actually valid regions
    of the class *)
