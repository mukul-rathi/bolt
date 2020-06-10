(** This module desugars the typed AST's function and class defns *)

open Typing

val desugar_function_defn :
     Typed_ast.class_defn list
  -> Typed_ast.function_defn list
  -> Typed_ast.function_defn
  -> Desugared_ast.function_defn

val desugar_class_defn :
     Typed_ast.class_defn list
  -> Typed_ast.function_defn list
  -> Typed_ast.class_defn
  -> Desugared_ast.class_defn
