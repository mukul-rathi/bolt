(** Type checks the consume operation - ensures you can't consume or access already
    consumed variables *)

open Core
open Desugaring.Desugared_ast

val type_consume_expr : expr -> identifier list -> identifier list Or_error.t
(** Takes as arguments the list of consumed variables before this expression, and returns
    the list of consumed variables after this expression is abstractly interpreted. *)
