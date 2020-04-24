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

let get_class_capabilities class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, capabilities, _, _)) -> capabilities

let get_class_field field_name (Typing.Typed_ast.TClass (_, _, field_defns, _)) =
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  List.hd_exn matching_field_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_capabilities class_name field_name class_defns =
  get_class_defn class_name class_defns
  |> fun (Typing.Typed_ast.TClass (_, capabilities, _, _) as class_defn) ->
  get_class_field field_name class_defn
  |> fun (TField (_, _, _, field_capability_names)) ->
  List.filter
    ~f:(fun (TCapability (_, capability_name)) ->
      elem_in_list capability_name field_capability_names)
    capabilities

let name_mangle_param_types param_types =
  String.concat
    (List.map
       ~f:(function
         | TEVoid             -> "v"
         | TEInt              -> "i"
         | TEBool             -> "b"
         | TEClass class_name ->
             let class_name_str = Class_name.to_string class_name in
             Fmt.str "%d%s" (String.length class_name_str) class_name_str)
       param_types)

let name_mangle_method meth_name param_types =
  Method_name.of_string
    (Fmt.str "_%s%s"
       (Method_name.to_string meth_name)
       (name_mangle_param_types param_types))

let name_mangle_function func_name param_types =
  Function_name.of_string
    (Fmt.str "_%s%s"
       (Function_name.to_string func_name)
       (name_mangle_param_types param_types))
