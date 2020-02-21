open Ast.Ast_types
open Core

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter
      ~f:(fun (Typing.Typed_ast.TClass (name, _, _, _)) -> class_name = name)
      class_defns in
  List.hd_exn matching_class_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let get_class_regions class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, regions, _, _)) -> regions

let get_class_field field_name (Typing.Typed_ast.TClass (_, _, field_defns, _)) =
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  List.hd_exn matching_field_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_regions class_name field_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  |> fun (TField (_, _, _, field_region_names)) ->
  List.filter
    ~f:(fun (TRegion (_, region_name)) -> elem_in_list region_name field_region_names)
    regions
