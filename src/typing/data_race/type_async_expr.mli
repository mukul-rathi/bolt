(** This module checks async expressions are data-race free, i.e. that the async
    expressions do not share mutable state. It also checks that objects with the "thread"
    capability are not accessed from outside their thread *)

open Core
open Ast_types
open Typed_ast

val type_async_expr : class_defn list -> trait_defn list -> expr -> unit Or_error.t
