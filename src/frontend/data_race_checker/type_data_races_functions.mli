(** This module is responsible for checking the the desugared AST functions for data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_function_defn :
     class_defn list
  -> function_defn list
  -> ignore_data_races:bool
  -> function_defn
  -> function_defn Or_error.t
(** If ignore_data_races flag set, will check capabilities but won't enforce constraints. *)
