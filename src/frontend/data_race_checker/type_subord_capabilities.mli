(** This module type-checks potential accesses to subordinate capabilities of objects *)

open Desugaring.Desugared_ast
open Ast.Ast_types
open Core

val type_subord_capabilities_block_expr :
  class_defn list -> obj_var_and_capabilities list -> block_expr -> block_expr

val type_subord_capabilities_method_prototype :
     class_defn list
  -> Class_name.t
  -> Method_name.t
  -> type_expr
  -> obj_var_and_capabilities list
  -> unit Or_error.t
(** type check any potential subordinate state passed into
    ([obj_var_and_capabilities list]) or returned from a method ([type_expr]) given the
    class the method belongs to. *)
