open Core

val ir_gen_expr :
     Data_race_checker.Data_race_checker_ast.class_defn list
  -> Data_race_checker.Data_race_checker_ast.expr
  -> Frontend_ir.expr Or_error.t
(** Generates the serialisable IR from a desugared expr *)

val ir_gen_block_expr :
     Data_race_checker.Data_race_checker_ast.class_defn list
  -> Data_race_checker.Data_race_checker_ast.block_expr
  -> Frontend_ir.expr list Or_error.t
(** Generates the serialisable IR from a desugared block expr *)
