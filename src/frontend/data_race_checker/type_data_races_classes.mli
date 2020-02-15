(** This module is responsible for checking the the desugared AST class definitions for
    data races *)

open Core
open Desugaring.Desugared_ast

val type_data_races_class_defn : class_defn list -> class_defn -> class_defn Or_error.t
