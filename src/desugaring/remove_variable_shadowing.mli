open Core
open Ast.Ast_types

type var_name_map = (Var_name.t * Var_name.t) list

val remove_var_shadowing :
     Typing.Typed_ast.expr
  -> var_name_map
  -> (Typing.Typed_ast.expr * var_name_map) Or_error.t
