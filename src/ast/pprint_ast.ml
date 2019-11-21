open Ast_types
open Format

let indent_space = "   "

let pprint_capability ppf indent cap =
  let string_of_cap =
    match cap with Linear -> "Linear" | Thread -> "Thread" | Read -> "Read" in
  fprintf ppf "%sCap: %s@." indent string_of_cap

let pprint_mode ppf indent mode =
  let string_of_mode = match mode with MConst -> "Const" | MVar -> "Var" in
  fprintf ppf "%sMode: %s@." indent string_of_mode

let pprint_cap_trait ppf indent (TCapTrait (cap, trait_name)) =
  fprintf ppf "%sCapTrait: %s@." indent (Trait_name.to_string trait_name) ;
  let new_indent = indent_space ^ indent in
  pprint_capability ppf new_indent cap

let pprint_type_field ppf indent tfield =
  let string_of_tfield = match tfield with TFieldInt -> "Int" in
  fprintf ppf "%sTField: %s@." indent string_of_tfield

let pprint_field_defn ppf indent (TField (mode, field_name, type_field)) =
  fprintf ppf "%sField Defn: %s@." indent (Field_name.to_string field_name) ;
  let new_indent = indent_space ^ indent in
  pprint_mode ppf new_indent mode ;
  pprint_type_field ppf new_indent type_field

let pprint_require_field_defn ppf indent (TRequire field_defn) =
  fprintf ppf "%sRequire@." indent ;
  let new_indent = indent_space ^ indent in
  pprint_field_defn ppf new_indent field_defn

let rec pprint_type_expr ppf indent type_expr =
  let print_texpr = fprintf ppf "%sType expr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match type_expr with
  | TEInt                      -> print_texpr "Int"
  | TEClass class_name         -> print_texpr ("Class " ^ Class_name.to_string class_name)
  | TECapTrait cap_trait       ->
      print_texpr "" ;
      pprint_cap_trait ppf new_indent cap_trait
  | TEFun (type_arg, type_res) ->
      print_texpr "Function" ;
      pprint_type_expr ppf new_indent type_arg ;
      pprint_type_expr ppf new_indent type_res

let pprint_class_defn ppf indent (TClass (class_name, cap_trait, field_defns)) =
  fprintf ppf "%sClass: %s@." indent (Class_name.to_string class_name) ;
  let new_indent = indent_space ^ indent in
  pprint_cap_trait ppf new_indent cap_trait ;
  List.iter (pprint_field_defn ppf new_indent) field_defns

let pprint_trait_defn ppf indent (TTrait (trait_name, cap, req_field_defns)) =
  fprintf ppf "%sTrait: %s@." indent (Trait_name.to_string trait_name) ;
  let new_indent = indent_space ^ indent in
  pprint_capability ppf new_indent cap ;
  List.iter (pprint_require_field_defn ppf new_indent) req_field_defns
