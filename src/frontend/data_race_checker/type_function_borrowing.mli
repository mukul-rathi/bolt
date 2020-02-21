open Core
open Desugaring.Desugared_ast
open Ast.Ast_types

val type_function_forward_borrowing_expr :
  class_defn list -> function_defn list -> expr -> unit Or_error.t

val type_function_forward_borrowing_block_expr :
  class_defn list -> function_defn list -> block_expr -> unit Or_error.t

val type_function_reverse_borrowing :
  class_defn list -> string -> type_expr -> block_expr -> unit Or_error.t
(** Checks the function return type [type_expr] and the [block_expr] body expression to
    check reverse borrowing. [string] is the error prefix *)
