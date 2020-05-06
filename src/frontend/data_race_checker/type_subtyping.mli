(** This module type-checks behavioural subtyping - that a class preserves the capability
    behaviour of its superclass. *)

open Core
open Ast.Ast_types
open Desugaring.Desugared_ast

val get_maybe_overridden_caps :
     class_defn list
  -> Class_name.t (** object class *)
  -> Class_name.t (** ref class *)
  -> capability list (** capabilities the ref assumes are used *)
  -> capability list
(** additional overridden capabilities that could be used *)

val type_subtyping : class_defn list -> class_defn -> unit Or_error.t
