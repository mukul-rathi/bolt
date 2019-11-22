open Ast_types
open Core
open Result

let check_no_duplicate_class_names class_defns =
  if
    List.contains_dup
      ~compare:(fun (TClass (name_1, _, _)) (TClass (name_2, _, _)) ->
        if name_1 = name_2 then 0 else 1)
      class_defns
  then
    Error
      (Error.of_string
         (Fmt.str "Duplicate class declarations. Classes must have distinct names.@."))
  else Ok ()

let check_req_field_present error_prefix
    (TRequire (TField (mode, trait_field_name, type_field))) class_fields =
  if
    List.exists
      ~f:(fun class_field -> class_field = TField (mode, trait_field_name, type_field))
      class_fields
  then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "%s missing required field: %s@." error_prefix
            (Field_name.to_string trait_field_name)))

let check_trait_req_fields_present error_prefix (TTrait (_name, _cap, req_field_defns))
    class_fields =
  List.fold ~init:(Ok ())
    ~f:(fun result_acc req_field_defn ->
      result_acc
      >>= fun () -> check_req_field_present error_prefix req_field_defn class_fields)
    req_field_defns

(* Check class's cap-trait is valid by seeing if there is a matching trait definition
   (same name and capability) which we then return *)
let check_valid_cap_trait error_prefix (TCapTrait (capability, trait_name)) trait_defns =
  let matching_trait_defns =
    List.filter
      ~f:(fun (TTrait (name, cap, _req_fd_defns)) ->
        name = trait_name && cap = capability)
      trait_defns in
  match matching_trait_defns with
  | []                  -> Error
                             (Error.of_string
                                (Fmt.str "%s No matching declarations.@." error_prefix))
  | [trait_defn] -> Ok trait_defn
  | _                   ->
      Error (Error.of_string (Fmt.str "%s Duplicate trait declarations.@." error_prefix))

let check_no_duplicate_fields error_prefix field_defns =
  if List.contains_dup ~compare:(fun x y -> if x = y then 0 else 1) field_defns then
    Error (Error.of_string (Fmt.str "%s Duplicate field declarations.@." error_prefix))
  else Ok ()

let type_class_defn
    (TClass (class_name, TCapTrait (capability, trait_name), class_fields) : class_defn)
    trait_defns =
  (* All type error strings for a particular class have same prefix *)
  let error_prefix = Fmt.str "%s has a type error: " (Class_name.to_string class_name) in
  check_no_duplicate_fields error_prefix class_fields
  >>= fun () ->
  (* Check class's cap-trait is valid *)
  check_valid_cap_trait error_prefix (TCapTrait (capability, trait_name)) trait_defns
  >>= function
  | matching_trait_defn ->
      check_trait_req_fields_present error_prefix matching_trait_defn class_fields

let type_class_defns class_defns trait_defns =
  check_no_duplicate_class_names class_defns
  >>= fun () ->
  Result.all_unit
    (List.map ~f:(fun class_defn -> type_class_defn class_defn trait_defns) class_defns)
