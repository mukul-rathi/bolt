open Core
open Type_data_races_expr
open Type_region_annotations
open Type_subord_regions
open Desugaring.Desugared_ast
open Ast.Ast_types
open Data_race_checker_env

let type_region_capability error_prefix (TRegion (cap, region_name)) =
  match cap with
  | Linear | Read | Locked | Thread | Subordinate -> Ok ()
  | Safe | Encapsulated ->
      Error
        (Error.of_string
           (Fmt.str "%s Region %s can't be assigned capability %s." error_prefix
              (Region_name.to_string region_name)
              (string_of_cap cap)))

let type_field_capability class_defns error_prefix (TRegion (region_cap, region_name))
    (TField (field_mode, field_type, field_name, _)) =
  match (region_cap, field_mode, field_type) with
  (* If a region has read capability then its fields must be const or have safe capability *)
  | Read, MVar, TEClass (field_class, _) ->
      if class_has_capability field_class Safe class_defns then Ok ()
      else
        Error
          (Error.of_string
             (Fmt.str
                "%s Field %s can't be in region %s as it doesn't have capability %s@."
                error_prefix
                (Field_name.to_string field_name)
                (Region_name.to_string region_name)
                (string_of_cap Safe)))
  | Read, MConst, _ -> Ok ()
  | _ -> Ok ()

let type_field_defn class_defns class_name regions error_prefix
    (TField (_, field_type, field_name, field_regions) as field_defn) =
  let open Result in
  ( match field_type with
  | TEClass (_, Borrowed) ->
      Error
        (Error.of_string
           (Fmt.str "%s Field %s can't be assigned a borrowed type." error_prefix
              (Field_name.to_string field_name)))
  | _                     -> Ok () )
  >>= fun () ->
  type_field_region_annotations class_name regions field_regions
  >>= fun field_regions ->
  Result.all_unit
    (List.map
       ~f:(fun region -> type_field_capability class_defns error_prefix region field_defn)
       field_regions)

(* check all fields in a region have the same type *)
let type_fields_region_types fields error_prefix (TRegion (_, reg_name)) =
  let region_fields =
    List.filter
      ~f:(fun (TField (_, _, _, field_regions)) ->
        List.exists ~f:(fun field_region -> field_region = reg_name) field_regions)
      fields in
  let field_types =
    List.map ~f:(fun (TField (_, field_type, _, _)) -> field_type) region_fields in
  match field_types with
  | []              ->
      Error
        (Error.of_string
           (Fmt.str "%s: region %s is unused@." error_prefix
              (Region_name.to_string reg_name)))
  | field_type :: _ ->
      if List.for_all ~f:(fun fd_type -> field_type = fd_type) field_types then Ok ()
      else
        Error
          (Error.of_string
             (Fmt.str "%sregion %s should have fields of the same type@." error_prefix
                (Region_name.to_string reg_name)))

let type_data_races_method_defn class_name class_defns
    (TMethod (method_name, ret_type, params, region_effects, body_expr)) =
  let open Result in
  let param_obj_var_regions = params_to_obj_vars_and_regions class_defns params in
  type_params_region_annotations class_defns params
  >>= fun () ->
  type_subord_regions_method_prototype class_defns class_name method_name ret_type
    param_obj_var_regions
  >>= fun () ->
  type_data_races_block_expr class_defns body_expr
    ((Var_name.of_string "this", class_name, region_effects) :: param_obj_var_regions)
  >>| fun data_race_checked_body_expr ->
  TMethod (method_name, ret_type, params, region_effects, data_race_checked_body_expr)

let type_data_races_class_defn class_defns
    (TClass (class_name, regions, fields, method_defns)) =
  let open Result in
  (* All type error strings for a particular class have same prefix *)
  let error_prefix = Fmt.str "%s has a type error: " (Class_name.to_string class_name) in
  Result.all_unit (List.map ~f:(type_region_capability error_prefix) regions)
  >>= fun () ->
  Result.all_unit (List.map ~f:(type_fields_region_types fields error_prefix) regions)
  >>= fun () ->
  Result.all
    (List.map ~f:(type_field_defn class_defns class_name regions error_prefix) fields)
  >>= fun _ ->
  Result.all
    (List.map ~f:(type_data_races_method_defn class_name class_defns) method_defns)
  >>| fun data_race_checked_method_defns ->
  TClass (class_name, regions, fields, data_race_checked_method_defns)
