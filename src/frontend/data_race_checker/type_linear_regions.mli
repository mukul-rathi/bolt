(** This module type-checks potential accesses to linear regions of objects *)

open Core
open Desugaring.Desugared_ast
open Ast.Ast_types

val type_linear_obj_method_args :
     class_defn list
  -> Var_name.t
  -> Class_name.t
  -> identifier list
  -> loc
  -> unit Or_error.t
(** Checks that linear objects are not passed as arguments to their methods. *)

val type_linear_args : class_defn list -> identifier list -> loc -> unit Or_error.t
(** Checks that linear arguments are not duplicated in function calls. *)

val type_linear_regions_block_expr :
  class_defn list -> block_expr -> block_expr Or_error.t
