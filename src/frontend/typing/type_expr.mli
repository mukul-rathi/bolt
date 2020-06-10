(** Infer type parsed expressions given the class and function definitions - return the
    expression annotated with types if type-checking succeeds. *)

open Ast.Ast_types
open Parsing
open Core
open Type_env

val type_expr :
     Parsed_ast.class_defn list
  -> Parsed_ast.function_defn list
  -> Parsed_ast.expr
  -> type_env
  -> (Typed_ast.expr * type_expr) Or_error.t

val type_block_expr :
     Parsed_ast.class_defn list
  -> Parsed_ast.function_defn list
  -> Parsed_ast.block_expr
  -> type_env
  -> (Typed_ast.block_expr * type_expr) Or_error.t
