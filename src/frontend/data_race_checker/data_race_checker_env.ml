open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _)) -> class_name = name) class_defns in
  match matching_class_defns with
  | [class_defn] -> Ok class_defn
  | _            ->
      Error
        (Error.of_string
           (Fmt.str
              "Something went wrong - couldn't get unique class definition for %s. @."
              (Class_name.to_string class_name)))

let get_class_regions class_name class_defns =
  let open Result in
  get_class_defn class_name class_defns >>| fun (TClass (_, regions, _, _)) -> regions

let get_class_field field_name (TClass (_, _, field_defns, _)) =
  let matching_class_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  match matching_class_defns with
  | [field] -> Ok field
  | _       ->
      Error
        (Error.of_string
           (Fmt.str
              "Something went wrong - couldn't get unique field definition for %s. @."
              (Field_name.to_string field_name)))

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_regions class_name field_name class_defns =
  let open Result in
  get_class_defn class_name class_defns
  >>= fun (TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  >>| fun (TField (_, _, _, field_region_names)) ->
  List.filter
    ~f:(fun (TRegion (_, region_name)) -> elem_in_list region_name field_region_names)
    regions
