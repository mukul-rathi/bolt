open Core
open Ast.Ast_types
open Type_env

let check_region_in_class_regions class_name class_regions region_name =
  if List.exists ~f:(fun (TRegion (_, name)) -> region_name = name) class_regions then
    Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "Error: region %s is not present in %s"
            (Region_name.to_string region_name)
            (Class_name.to_string class_name)))

let type_method_effect_region_annotations class_name class_regions method_effect_regions =
  Result.all_unit
    (List.map
       ~f:(check_region_in_class_regions class_name class_regions)
       method_effect_regions)

let type_param_region_annotations class_defns = function
  | TParam (param_type, _, optional_region_guards) -> (
    match param_type with
    | TEClass obj_class       -> (
        let open Result in
        match optional_region_guards with
        | Some region_guards ->
            get_class_regions obj_class class_defns
            >>= fun class_regions ->
            Result.all_unit
              (List.map
                 ~f:(check_region_in_class_regions obj_class class_regions)
                 region_guards)
        | None               -> Ok () )
    | TEInt | TEBool | TEVoid -> Ok () )
  | TVoid -> Ok ()

let type_params_region_annotations class_defns params =
  Result.all_unit (List.map ~f:(type_param_region_annotations class_defns) params)
