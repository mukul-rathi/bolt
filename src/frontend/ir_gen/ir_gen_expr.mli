open Core

val ir_gen_expr :
     Desugaring.Desugared_ast.class_defn list
  -> Desugaring.Desugared_ast.expr
  -> Frontend_ir.expr Or_error.t
(** Generates the serialisable IR from a desugared expr *)
