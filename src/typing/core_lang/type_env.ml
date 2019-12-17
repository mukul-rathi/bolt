open Ast.Ast_types
open Core
open Result

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

let check_type_equality type_expr_1 type_expr_2 =
  (* Structural equality on types *) type_expr_1 = type_expr_2

let field_to_expr_type TFieldInt = TEInt

(********** GETTER METHODS for type-checking core language *********)

let rec get_var_type (var_name : Var_name.t) (env : type_env) loc =
  match env with
  | []                            ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Variable not defined in environment@."
              (string_of_loc loc)))
  | (var_name', var_type) :: env' ->
      if var_name' = var_name then Ok var_type else get_var_type var_name env' loc

let get_class_defn class_name class_defns loc =
  let matching_class_defns =
    List.filter
      ~f:(fun (Parsing.Parsed_ast.TClass (name, _, _, _)) -> class_name = name)
      class_defns in
  match matching_class_defns with
  | []           ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Class %s not defined in environment@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))
  | [class_defn] -> Ok class_defn
  | _            ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Class %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))

let get_class_field field_name (Parsing.Parsed_ast.TClass (_, _, field_defns, _)) loc =
  let matching_class_defns =
    List.filter ~f:(fun (TField (_, name, _)) -> field_name = name) field_defns in
  match matching_class_defns with
  | []      ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Field %s not defined in environment@."
              (string_of_loc loc)
              (Field_name.to_string field_name)))
  | [field] -> Ok field
  | _       ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Field %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Field_name.to_string field_name)))

let get_obj_class_defn var_name env class_defns loc =
  get_var_type var_name env loc
  >>= function
  | TEClass class_name -> get_class_defn class_name class_defns loc
  | wrong_type         ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - %s should be an object, instead is of type %s@."
              (string_of_loc loc) (Var_name.to_string var_name)
              (string_of_type wrong_type)))

let get_function_type func_name function_defns loc =
  let matching_function_defns =
    List.filter
      ~f:(fun (Parsing.Parsed_ast.TFunction (name, _, _, _)) -> func_name = name)
      function_defns in
  match matching_function_defns with
  | [] ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Function %s not defined in environment@."
              (string_of_loc loc)
              (Function_name.to_string func_name)))
  | [Parsing.Parsed_ast.TFunction (_, return_type, params, _)] ->
      let param_types = List.map ~f:(fun (TParam (param_type, _)) -> param_type) params in
      Ok (param_types, return_type)
  | _ ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - Function %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Function_name.to_string func_name)))

let get_function_body_expr func_name function_defns loc =
  let matching_function_defns =
    List.filter
      ~f:(fun (Typed_ast.TFunction (name, _, _, _)) -> func_name = name)
      function_defns in
  match matching_function_defns with
  | [] ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Function %s not defined in environment@."
              (string_of_loc loc)
              (Function_name.to_string func_name)))
  | [Typed_ast.TFunction (_, _, _, body_expr)] -> Ok body_expr
  | _ ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - Function %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Function_name.to_string func_name)))

(********** GETTER METHODS for type-checking data races *********)

let get_type_capability type_expr class_defns loc =
  match type_expr with
  | TEClass class_name -> (
      let matching_class_defns =
        List.filter
          ~f:(fun (Typed_ast.TClass (name, _, _, _)) -> class_name = name)
          class_defns in
      match matching_class_defns with
      | [] ->
          Error
            (Error.of_string
               (Fmt.str "%s Type error - Class %s not defined in environment@."
                  (string_of_loc loc)
                  (Class_name.to_string class_name)))
      | [Typed_ast.TClass (_, TCapTrait (cap, _), _, _)] -> Ok cap
      | _ ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - Class %s has duplicate definitions in environment@."
                  (string_of_loc loc)
                  (Class_name.to_string class_name))) )
  | TECapTrait (TCapTrait (cap, _)) -> Ok cap
  | _ ->
      Error
        (Error.of_string
           (Fmt.str "%s Type doesn't contain capability" (string_of_loc loc)))
