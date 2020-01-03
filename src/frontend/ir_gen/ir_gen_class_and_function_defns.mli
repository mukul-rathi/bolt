(** Generates serialisable IR representations of the desugared class and function
    definitions *)

open Core

val ir_gen_class_defns :
  Desugaring.Desugared_ast.class_defn list -> Llvm_ast.class_defn list

val ir_gen_function_defns :
     Desugaring.Desugared_ast.class_defn list
  -> Desugaring.Desugared_ast.function_defn list
  -> Llvm_ast.function_defn list Or_error.t
(** Generates IR function defns from the function defns and each class's method defns *)
