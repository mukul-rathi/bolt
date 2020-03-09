(** This module is responsible for checking the the desugared AST class definitions for
    data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_class_defn :
     class_defn list
  -> function_defn list
  -> ignore_data_races:bool
  -> class_defn
  -> class_defn Or_error.t
(** If ignore_data_races flag set, will check capabilities but won't enforce constraints. *)
