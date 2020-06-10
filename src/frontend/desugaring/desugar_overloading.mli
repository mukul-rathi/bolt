(** Desugar overloaded functions and methods *)

open Ast.Ast_types
open Typing

val name_mangle_overloaded_method : Method_name.t -> type_expr list -> Method_name.t

val name_mangle_if_overloaded_function :
  Typed_ast.function_defn list -> Function_name.t -> type_expr list -> Function_name.t
