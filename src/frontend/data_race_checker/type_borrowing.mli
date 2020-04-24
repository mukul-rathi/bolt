open Core
open Desugaring.Desugared_ast
open Ast.Ast_types

val type_function_forward_borrowing_expr :
  class_defn list -> function_defn list -> expr -> unit Or_error.t
(** check an identifier isn't borrowed multiple times when passed as args to function
    calls (forward borrowing) *)

val type_function_forward_borrowing_block_expr :
  class_defn list -> function_defn list -> block_expr -> unit Or_error.t

val type_function_reverse_borrowing :
     class_defn list
  -> string
  -> type_expr
  -> borrowed_ref option
  -> block_expr
  -> unit Or_error.t
(** Checks the function return type [type_expr] to determine if linear reference, whether
    it returns borrowed refernce [borrowed_ref option] and if the [block_expr] body
    expression returns a non-consumed id (not allowed if no reverse borrowing in function
    signature). [string] is the error prefix *)

val type_assign_borrowed_expr :
  class_defn list -> function_defn list -> expr -> borrowed_ref option Or_error.t
(** Checks that borrowed values aren't assigned to objects (so don't outlive their
    borrowed scope.) Returns whether the current expr / block reduces to a borrowed value. *)

val type_assign_borrowed_block_expr :
  class_defn list -> function_defn list -> block_expr -> borrowed_ref option Or_error.t
