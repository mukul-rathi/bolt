open Ast.Ast_types
open Core
open Type_expr
open Type_region_annotations

let check_no_duplicate_class_names class_defns =
  if
    List.contains_dup
      ~compare:
        (fun (Parsing.Parsed_ast.TClass (name_1, _, _, _))
             (Parsing.Parsed_ast.TClass (name_2, _, _, _)) ->
        if name_1 = name_2 then 0 else 1)
      class_defns
  then
    Error
      (Error.of_string
         (Fmt.str "Duplicate class declarations. Classes must have distinct names.@."))
  else Ok ()

let check_no_duplicate_fields error_prefix field_defns =
  if
    List.contains_dup
      ~compare:(fun (TField (_, _, name_1, _)) (TField (_, _, name_2, _)) ->
        if name_1 = name_2 then 0 else 1)
      field_defns
  then Error (Error.of_string (Fmt.str "%s Duplicate field declarations.@." error_prefix))
  else Ok ()

let type_field_defn class_name regions error_prefix
    (TField (_, field_type, field_name, field_regions)) =
  let open Result in
  ( match field_type with
  | TEClass (_, Borrowed) ->
      Error
        (Error.of_string
           (Fmt.str "%s Field %s can't be assigned a borrowed type." error_prefix
              (Field_name.to_string field_name)))
  | _                     -> Ok () )
  >>= fun () -> type_intra_class_region_annotations class_name regions field_regions

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

(* Type check method bodies *)

let init_env_from_method_params params class_name =
  let param_env =
    List.map
      ~f:(function TParam (type_expr, param_name, _) -> (param_name, type_expr))
      params in
  (Var_name.of_string "this", TEClass (class_name, Borrowed)) :: param_env

let type_method_defn class_defns function_defns class_name class_regions
    (Parsing.Parsed_ast.TMethod
      (method_name, return_type, params, region_effect_names, body_expr)) =
  let open Result in
  type_params_region_annotations class_defns params
  >>= fun () ->
  type_intra_class_region_annotations class_name class_regions region_effect_names
  >>= fun region_effects ->
  type_block_expr class_defns function_defns body_expr
    (init_env_from_method_params params class_name)
  >>= fun (typed_body_expr, body_return_type) ->
  (* We throw away returned expr if return type is void *)
  if return_type = TEVoid || body_return_type = return_type then
    Ok
      (Typed_ast.TMethod
         (method_name, return_type, params, region_effects, typed_body_expr))
  else
    Error
      (Error.of_string
         (Fmt.str
            "Type Error for method %s: expected return type of %s but got %s instead"
            (Method_name.to_string method_name)
            (string_of_type return_type)
            (string_of_type body_return_type)))

(* Check a given class definition is well formed *)
let type_class_defn
    (Parsing.Parsed_ast.TClass (class_name, regions, class_fields, method_defns))
    class_defns function_defns =
  let open Result in
  (* All type error strings for a particular class have same prefix *)
  let error_prefix = Fmt.str "%s has a type error: " (Class_name.to_string class_name) in
  check_no_duplicate_fields error_prefix class_fields
  >>= fun () ->
  Result.all_unit
    (List.map ~f:(type_fields_region_types class_fields error_prefix) regions)
  >>= fun () ->
  Result.all (List.map ~f:(type_field_defn class_name regions error_prefix) class_fields)
  >>= fun _ ->
  Result.all
    (List.map
       ~f:(type_method_defn class_defns function_defns class_name regions)
       method_defns)
  >>| fun typed_method_defns ->
  Typed_ast.TClass (class_name, regions, class_fields, typed_method_defns)

(* Check all class definitions are well formed *)
let type_class_defns class_defns function_defns =
  let open Result in
  check_no_duplicate_class_names class_defns
  >>= fun () ->
  Result.all
    (List.map
       ~f:(fun class_defn -> type_class_defn class_defn class_defns function_defns)
       class_defns)
