open Ast_types
open Core

let indent_space = "   "

let pprint_modifier ppf ~indent modifier =
  Fmt.pf ppf "%sModifier: %s@." indent (string_of_modifier modifier)

let pprint_type_expr ppf ~indent type_expr =
  Fmt.pf ppf "%sType expr: %s@." indent (string_of_type type_expr)

let pprint_capability ppf ~indent (TCapability (mode, capability_name)) =
  Fmt.pf ppf "%sCapability: %s %s@." indent (string_of_mode mode)
    (Capability_name.to_string capability_name)

let pprint_capabilities ppf ~indent capabilities =
  Fmt.pf ppf "%sCapabilities: @." indent ;
  let new_indent = indent_space ^ indent in
  List.iter ~f:(pprint_capability ppf ~indent:new_indent) capabilities

let pprint_capability_names ppf ~indent capability_names =
  Fmt.pf ppf "%sCapabilities: %s@." indent
    (String.concat ~sep:"," (List.map ~f:Capability_name.to_string capability_names))

let pprint_field_defn ppf ~indent
    (TField (modifier, type_field, field_name, capability_names)) =
  Fmt.pf ppf "%sField Defn: %s@." indent (Field_name.to_string field_name) ;
  let new_indent = indent_space ^ indent in
  pprint_modifier ppf ~indent:new_indent modifier ;
  pprint_type_expr ppf ~indent:new_indent type_field ;
  pprint_capability_names ppf ~indent:new_indent capability_names

let pprint_param ppf ~indent = function
  | TParam (type_expr, param_name, maybe_capabilities_restricted, maybe_borrowed) -> (
      let string_of_maybe_borrowed =
        match maybe_borrowed with Some Borrowed -> "Borrowed " | None -> "" in
      Fmt.pf ppf "%s%sParam: %s@." indent string_of_maybe_borrowed
        (Var_name.to_string param_name) ;
      let new_indent = indent_space ^ indent in
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      match maybe_capabilities_restricted with
      | Some allowed_capability_names ->
          pprint_capability_names ppf ~indent:new_indent allowed_capability_names
      | None                          -> () )

let pprint_params ppf ~indent = function
  | []     -> Fmt.pf ppf "%sParam: %s@." indent (string_of_type TEVoid)
  | params -> List.iter ~f:(pprint_param ppf ~indent) params
