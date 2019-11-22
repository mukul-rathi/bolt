open Ast_types
open Core
open Result

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

let check_type_equality type_expr_1 type_expr_2 =
  (* Structural equality on types *) type_expr_1 = type_expr_2

let field_to_expr_type TFieldInt = TEInt

let rec get_var_type (var_name : Var_name.t) (env : type_env) loc =
  match env with
  | []                               ->
      Error
        (Error.of_string
           (sprintf "%s Type error - Variable not defined in environment@."
              (string_of_loc loc)))
  | (var_name', var_type) :: env' ->
      if var_name' = var_name then Ok var_type else get_var_type var_name env' loc

let get_class_defn class_name class_defns loc =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _)) -> class_name = name) class_defns in
  match matching_class_defns with
  | []                  ->
      Error
        (Error.of_string
           (sprintf "%s Type error - Class %s not defined in environment@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))
  | [class_defn] -> Ok class_defn
  | _                   ->
      Error
        (Error.of_string
           (sprintf "%s Type error - Class %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))

let get_field_type field_name (TClass (_, _, field_defns)) loc =
  let matching_class_defns =
    List.filter ~f:(fun (TField (_, name, _)) -> field_name = name) field_defns in
  match matching_class_defns with
  | []                                   ->
      Error
        (Error.of_string
           (sprintf "%s Type error - Field %s not defined in environment@."
              (string_of_loc loc)
              (Field_name.to_string field_name)))
  | [TField (_, _, field_type)] -> Ok field_type
  | _                                    ->
      Error
        (Error.of_string
           (sprintf "%s Type error - Field %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Field_name.to_string field_name)))
