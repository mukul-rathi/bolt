open Core
open Ast.Ast_types

val ir_gen_method_name : Method_name.t -> type_expr -> string Or_error.t
(** Name mangling of method names - takes as input the method name and the type of the
    object calling it and returns the mangled name *)

val ir_gen_field_index :
  Field_name.t -> type_expr -> Desugaring.Desugared_ast.class_defn list -> int Or_error.t
(** Given a field and the type of the object to which it belongs, and a list of class
    defns, get the field index within the list of field defns in the corresponding class
    defn *)
