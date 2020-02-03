(** Generates serialisable IR representations of the desugared class and function
    definitions *)

open Core

val ir_gen_class_defns :
  Data_race_checker.Data_race_checker_ast.class_defn list -> Frontend_ir.class_defn list

val ir_gen_function_defns :
     Data_race_checker.Data_race_checker_ast.class_defn list
  -> Data_race_checker.Data_race_checker_ast.function_defn list
  -> Frontend_ir.function_defn list Or_error.t
(** Generates IR function defns from the function defns and each class's method defns *)
