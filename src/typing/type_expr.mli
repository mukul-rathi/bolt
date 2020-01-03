(** Infer type parsed expressions given the class and function definitions - return the
    expression annotated with types if type-checking succeeds. *)

open Ast.Ast_types
open Core
open Type_env

val infer_type_expr :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.function_defn list
  -> Parsing.Parsed_ast.expr
  -> type_env
  -> (Typed_ast.expr * type_expr) Or_error.t

val type_expr :
     Parsing.Parsed_ast.class_defn list
  -> Parsing.Parsed_ast.function_defn list
  -> Parsing.Parsed_ast.expr
  -> Typed_ast.expr Or_error.t
