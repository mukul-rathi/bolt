open Core
open Ast.Ast_types

val ir_gen_method_name : Method_name.t -> type_expr -> string Or_error.t
(** Name mangling of method names - takes as input the method name and the type of the
    object calling it and returns the mangled name *)

val ir_gen_expr : Desugaring.Desugared_ast.expr -> Frontend_ir.expr Or_error.t
(** Generates the serialisable IR from a desugared expr *)
