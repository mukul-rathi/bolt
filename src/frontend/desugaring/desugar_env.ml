open Ast.Ast_types
open Typing
open Core

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter
      ~f:(fun (Typed_ast.TClass (name, _, _, _, _, _)) -> class_name = name)
      class_defns in
  List.hd_exn matching_class_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec get_class_method_defns class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typed_ast.TClass (_, _, maybe_inherits, _, _, method_defns)) ->
  ( match maybe_inherits with
  | Some superclass -> get_class_method_defns superclass class_defns
  | None            -> [] )
  |> fun superclass_methods ->
  List.concat [superclass_methods; method_defns]
  (* filter out overriden methods (i.e those with same name and params) *)
  |> List.dedup_and_sort
       ~compare:(fun (Typed_ast.TMethod (name_1, _, _, params_1, _, _))
                     (Typed_ast.TMethod (name_2, _, _, params_2, _, _))
                     ->
         if name_1 = name_2 && get_params_types params_1 = get_params_types params_2 then
           0
         else 1)

let rec get_class_capabilities class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typed_ast.TClass (_, _, maybe_inherits, capabilities, _, _)) ->
  ( match maybe_inherits with
  | Some super_class -> get_class_capabilities super_class class_defns
  | None             -> [] )
  |> fun superclass_caps -> List.concat [superclass_caps; capabilities]

let rec get_class_fields class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typed_ast.TClass (_, _, maybe_inherits, _, field_defns, _)) ->
  ( match maybe_inherits with
  | Some super_class -> get_class_fields super_class class_defns
  | None             -> [] )
  |> fun superclass_fields -> List.concat [superclass_fields; field_defns]

let get_class_field class_defns class_name field_name =
  let matching_field_defns =
    List.filter
      ~f:(fun (TField (_, _, name, _)) -> field_name = name)
      (get_class_fields class_name class_defns) in
  List.hd_exn matching_field_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_capabilities class_name field_name class_defns =
  get_class_field class_defns class_name field_name
  |> fun (TField (_, _, _, field_capability_names)) ->
  get_class_capabilities class_name class_defns
  |> fun capabilities ->
  List.filter
    ~f:(fun (TCapability (_, capability_name)) ->
      elem_in_list capability_name field_capability_names)
    capabilities

let maybe_get_superclass class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typed_ast.TClass (_, _, maybe_inherits, _, _, _)) -> maybe_inherits
