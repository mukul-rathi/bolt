(** This module types whether capabilities can be concurrently accessed *)

open Ast.Ast_types
open Desugaring.Desugared_ast
open Core

val can_concurrently_access_capabilities :
  Class_name.t -> class_defn list -> capability -> capability -> bool
(** Checks if the two capabilities of the object of the given class can be used
    concurrently *)

val type_concurrent_capability_constraints_vars :
  class_defn list -> obj_var_and_capabilities list -> loc -> unit Or_error.t
(** This function takes a list of free variables and types concurrent accesses of
    variables (it is expected that each occurrence of a given free variable corresponds to
    a separate thread). *)
