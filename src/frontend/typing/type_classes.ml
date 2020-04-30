open Ast.Ast_types
open Core
open Type_expr
open Type_env
open Type_overloading
open Type_generics
open Type_inheritance

let check_no_duplicate_class_names class_defns =
  if
    List.contains_dup
      ~compare:
        (fun (Parsing.Parsed_ast.TClass (name_1, _, _, _, _, _))
             (Parsing.Parsed_ast.TClass (name_2, _, _, _, _, _)) ->
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

(* Type check method bodies *)

let init_env_from_method_params params class_defn =
  let param_env =
    List.map
      ~f:(function TParam (type_expr, param_name, _, _) -> (param_name, type_expr))
      params in
  instantiate_maybe_generic_this class_defn :: param_env

let type_method_defn class_defns function_defns
    (Parsing.Parsed_ast.TClass (class_name, _, _, _, _, _) as current_class_defn)
    (Parsing.Parsed_ast.TMethod
      ( method_name
      , maybe_borrow_ref_ret
      , return_type
      , params
      , used_capability_names
      , body_expr )) =
  let open Result in
  get_class_capabilities class_name class_defns
  >>= fun class_capabilities ->
  get_method_capability_annotations class_name class_capabilities used_capability_names
  >>= fun capabilities_used ->
  type_generics_usage_class_defn current_class_defn
  >>= fun () ->
  type_block_expr class_defns function_defns body_expr
    (init_env_from_method_params params current_class_defn)
  >>= fun (typed_body_expr, body_return_type) ->
  (* We throw away returned expr if return type is void *)
  if return_type = TEVoid || is_subtype_of body_return_type return_type then
    Ok
      (Typed_ast.TMethod
         ( method_name
         , maybe_borrow_ref_ret
         , return_type
         , params
         , capabilities_used
         , typed_body_expr ))
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
    ( Parsing.Parsed_ast.TClass
        ( class_name
        , maybe_generic
        , maybe_inherits
        , capabilities
        , class_fields
        , method_defns ) as current_class_defn ) class_defns function_defns =
  let open Result in
  (* All type error strings for a particular class have same prefix *)
  let error_prefix = Fmt.str "%s has a type error: " (Class_name.to_string class_name) in
  check_no_duplicate_fields error_prefix class_fields
  >>= fun () ->
  type_overloaded_method_defns method_defns
  >>= fun () ->
  type_class_inheritance current_class_defn class_defns
  >>= fun () ->
  Result.all
    (List.map
       ~f:(type_method_defn class_defns function_defns current_class_defn)
       method_defns)
  >>| fun typed_method_defns ->
  Typed_ast.TClass
    ( class_name
    , maybe_generic
    , maybe_inherits
    , capabilities
    , class_fields
    , typed_method_defns )

(* Check all class definitions are well formed *)
let type_class_defns class_defns function_defns =
  let open Result in
  check_no_duplicate_class_names class_defns
  >>= fun () ->
  Result.all
    (List.map
       ~f:(fun class_defn -> type_class_defn class_defn class_defns function_defns)
       class_defns)
