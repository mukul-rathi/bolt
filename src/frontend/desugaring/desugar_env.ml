open Ast.Ast_types
open Core

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter
      ~f:(fun (Typing.Typed_ast.TClass (name, _, _, _, _)) -> class_name = name)
      class_defns in
  List.hd_exn matching_class_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let get_class_method_defns class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, _, _, _, method_defns)) -> method_defns

let get_class_capabilities class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, _, capabilities, _, _)) -> capabilities

let get_class_field field_name (Typing.Typed_ast.TClass (_, _, _, field_defns, _)) =
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  List.hd_exn matching_field_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_capabilities class_name field_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, _, capabilities, _, _) as class_defn) ->
  get_class_field field_name class_defn
  |> fun (TField (_, _, _, field_capability_names)) ->
  List.filter
    ~f:(fun (TCapability (_, capability_name)) ->
      elem_in_list capability_name field_capability_names)
    capabilities
