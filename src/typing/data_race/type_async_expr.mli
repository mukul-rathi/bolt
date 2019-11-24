(** This module checks thread references are not accessed from another thread *)

open Core
open Ast_types
open Typed_ast
open Type_env

val type_async_expr :
  class_defn list -> trait_defn list -> expr -> (type_env, Error.t) Result.t
(** The "Ok" result type - type_env is not used at all when the function returns, it is
    only used when the function makes recursive calls to itself *)
