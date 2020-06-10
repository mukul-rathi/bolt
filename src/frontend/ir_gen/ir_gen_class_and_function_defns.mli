(** Generates serialisable IR representations of the desugared class and function
    definitions *)

open Core
open Desugaring

val ir_gen_class_defns : Desugared_ast.class_defn list -> Frontend_ir.class_defn list

val ir_gen_function_defns :
     Desugared_ast.class_defn list
  -> Desugared_ast.function_defn list
  -> Frontend_ir.function_defn list Or_error.t
(** Generates IR function defns from the function defns and each class's method defns *)
