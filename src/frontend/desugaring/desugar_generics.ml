open Count_generics_instantiations
open Name_mangle_generics
open Replace_generic_with_instantiated_class_defns
open Typing

(* First, count the number of concrete type parameters a generic class has been
   instantiated with *)

(* NEXT, instantiate classes with concrete types *)

(* Name mangle types so pointing to correct concrete class instantiation *)

let desugar_generics_program
    (Typed_ast.Prog (class_defns, function_defns, main_expr) as prog) =
  count_generics_instantiations_program prog
  |> fun class_insts ->
  replace_generic_with_instantiated_class_defns class_defns class_insts
  |> fun instantiated_class_defns ->
  name_mangle_generics_usage_program
    (Typed_ast.Prog (instantiated_class_defns, function_defns, main_expr))
