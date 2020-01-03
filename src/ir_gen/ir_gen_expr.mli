open Core
open Ast.Ast_types

val ir_gen_method_name : Method_name.t -> type_expr -> string Or_error.t
(** Name mangling of method names *)

val ir_gen_expr : Desugaring.Desugared_ast.expr -> Llvm_ast.expr Or_error.t
