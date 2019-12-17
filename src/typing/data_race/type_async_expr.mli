(** This module checks async expressions are data-race free, i.e. that the async
    expressions do not share mutable state. It also checks that objects with the "thread"
    capability are not accessed from outside their thread *)

open Core
open Typing_core_lang.Typed_ast

val type_program_async_exprs : program -> unit Or_error.t
