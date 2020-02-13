(** This module contains helper functions used in the frontend IR generation stage *)

open Core
open Ast.Ast_types

val ir_gen_method_name : Method_name.t -> Class_name.t -> string
(** Name mangling of method names - takes as input the method name and the class of the
    object calling it and returns the mangled name *)

val ir_gen_field_index :
     Field_name.t
  -> Class_name.t
  -> Data_race_checker.Data_race_checker_ast.class_defn list
  -> int Or_error.t
(** Given a field and the type of the object to which it belongs, and a list of class
    defns, get the field index within the list of field defns in the corresponding class
    defn *)
