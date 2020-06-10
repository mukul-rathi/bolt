open Desugaring

val ir_gen_expr : Desugared_ast.class_defn list -> Desugared_ast.expr -> Frontend_ir.expr
(** Generates the serialisable IR from a desugared expr *)

val ir_gen_block_expr :
  Desugared_ast.class_defn list -> Desugared_ast.block_expr -> Frontend_ir.expr list
(** Generates the serialisable IR from a desugared block expr *)
