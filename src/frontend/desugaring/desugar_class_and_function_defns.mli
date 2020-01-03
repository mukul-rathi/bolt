(** This module desugars the typed AST's function and class defns *)

open Core

val desugar_function_defn :
  Typing.Typed_ast.function_defn -> Desugared_ast.function_defn Or_error.t

val desugar_class_defn :
  Typing.Typed_ast.class_defn -> Desugared_ast.class_defn Or_error.t
