open Ast_types
open Core

let indent_space = "   "
let pprint_mode ppf ~indent mode = Fmt.pf ppf "%sMode: %s@." indent (string_of_mode mode)

let pprint_type_expr ppf ~indent type_expr =
  Fmt.pf ppf "%sType expr: %s@." indent (string_of_type type_expr)

let pprint_region ppf ~indent (TRegion (cap, region_name)) =
  Fmt.pf ppf "%sRegion: %s %s@." indent (string_of_cap cap)
    (Region_name.to_string region_name)

let pprint_regions ppf ~indent regions =
  Fmt.pf ppf "%sRegions: @." indent ;
  let new_indent = indent_space ^ indent in
  List.iter ~f:(pprint_region ppf ~indent:new_indent) regions

let pprint_region_names ppf ~indent region_names =
  Fmt.pf ppf "%sRegions: %s@." indent
    (String.concat ~sep:"," (List.map ~f:Region_name.to_string region_names))

let pprint_field_defn ppf ~indent (TField (mode, type_field, field_name, region_names)) =
  Fmt.pf ppf "%sField Defn: %s@." indent (Field_name.to_string field_name) ;
  let new_indent = indent_space ^ indent in
  pprint_mode ppf ~indent:new_indent mode ;
  pprint_type_expr ppf ~indent:new_indent type_field ;
  pprint_region_names ppf ~indent:new_indent region_names

let pprint_param ppf ~indent = function
  | TParam (type_expr, param_name, maybe_region_guard) -> (
      Fmt.pf ppf "%sParam: %s@." indent (Var_name.to_string param_name) ;
      let new_indent = indent_space ^ indent in
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      match maybe_region_guard with
      | Some region_names -> pprint_region_names ppf ~indent:new_indent region_names
      | None              -> () )

let pprint_params ppf ~indent = function
  | []     -> Fmt.pf ppf "%sParam: %s@." indent (string_of_type TEVoid)
  | params -> List.iter ~f:(pprint_param ppf ~indent) params
