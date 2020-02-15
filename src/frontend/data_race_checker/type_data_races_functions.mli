(** This module is responsible for checking the the desugared AST functions for data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_function_defn :
  class_defn list -> function_defn -> function_defn Or_error.t
