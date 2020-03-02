open Core
open Ast.Ast_types
open Data_race_checker_env

let check_capability_in_class_capabilities class_name class_capabilities capability_name =
  match
    List.filter
      ~f:(fun (TCapability (_, name)) -> capability_name = name)
      class_capabilities
  with
  | []              ->
      Error
        (Error.of_string
           (Fmt.str "Error: capability %s is not present in %s"
              (Capability_name.to_string capability_name)
              (Class_name.to_string class_name)))
  | capability :: _ -> Ok capability

let type_field_capability_annotations class_name class_capabilities capability_names =
  Result.all
    (List.map
       ~f:(check_capability_in_class_capabilities class_name class_capabilities)
       capability_names)

let type_param_capability_annotations class_defns = function
  | TParam (param_type, _, optional_capability_guards) -> (
      let open Result in
      match param_type with
      | TEClass (obj_class, _)  -> (
          let open Result in
          match optional_capability_guards with
          | Some capability_guards ->
              get_class_capabilities obj_class class_defns
              |> fun class_capabilities ->
              Result.all
                (List.map
                   ~f:
                     (check_capability_in_class_capabilities obj_class class_capabilities)
                   capability_guards)
              >>| fun _ -> ()
          | None                   -> Ok () )
      | TEInt | TEBool | TEVoid -> Ok () )

let type_params_capability_annotations class_defns params =
  Result.all_unit (List.map ~f:(type_param_capability_annotations class_defns) params)
