open Core

val ir_gen_expr :
     Desugaring.Desugared_ast.class_defn list
  -> Desugaring.Desugared_ast.expr
  -> Frontend_ir.expr Or_error.t
(** Generates the serialisable IR from a desugared expr *)

val ir_gen_block_expr :
     Desugaring.Desugared_ast.class_defn list
  -> Desugaring.Desugared_ast.block_expr
  -> Frontend_ir.expr list Or_error.t
(** Generates the serialisable IR from a desugared block expr *)
