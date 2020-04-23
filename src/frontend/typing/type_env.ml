open Ast.Ast_types
open Core

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

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

let get_class_capabilities class_name class_defns =
  let open Result in
  get_class_defn class_name class_defns Lexing.dummy_pos
  >>| fun (Parsing.Parsed_ast.TClass (_, capabilities, _, _)) -> capabilities

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

let get_method_capability_annotations class_name class_capabilities capability_names =
  Result.all
    (List.map
       ~f:(check_capability_in_class_capabilities class_name class_capabilities)
       capability_names)

let get_class_field field_name (Parsing.Parsed_ast.TClass (_, _, field_defns, _)) loc =
  let matching_class_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
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
  let open Result in
  get_var_type var_name env loc
  >>= function
  | TEClass (class_name, _) -> get_class_defn class_name class_defns loc
  | wrong_type              ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - %s should be an object, instead is of type %s@."
              (string_of_loc loc) (Var_name.to_string var_name)
              (string_of_type wrong_type)))

let get_param_types = function
  | []     -> [TEVoid]
  | params -> List.map ~f:(function TParam (param_type, _, _) -> param_type) params

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
      Ok (get_param_types params, return_type)
  | _ ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - Function %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Function_name.to_string func_name)))

let get_method_type method_name (Parsing.Parsed_ast.TClass (_, _, _, method_defns)) loc =
  let matching_method_defns =
    List.filter
      ~f:(fun (Parsing.Parsed_ast.TMethod (name, _, _, _, _)) -> method_name = name)
      method_defns in
  match matching_method_defns with
  | [] ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Method %s not defined in environment@."
              (string_of_loc loc)
              (Method_name.to_string method_name)))
  | [Parsing.Parsed_ast.TMethod (_, return_type, params, _, _)] ->
      Ok (get_param_types params, return_type)
  | _ ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Method %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Method_name.to_string method_name)))

(********** CHECK METHODS for checking invariants *********)

let check_no_var_shadowing_in_block exprs loc =
  if
    List.contains_dup
      ~compare:(fun expr1 expr2 ->
        match expr1 with
        | Parsing.Parsed_ast.Let (_, _, var_name1, _) -> (
          match expr2 with
          | Parsing.Parsed_ast.Let (_, _, var_name2, _) ->
              if var_name1 = var_name2 then 0 (* duplicate let binding! *) else 1
          | _ -> 1 )
        | _ -> 1)
      exprs
  then
    Error
      (Error.of_string
         (Fmt.str "%s Type error: Duplicate variable declarations in same block.@."
            (string_of_loc loc)))
  else Ok ()

let check_identifier_assignable class_defns id env loc =
  let open Result in
  match id with
  | Parsing.Parsed_ast.Variable x ->
      if x = Var_name.of_string "this" then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Assigning expr to 'this'.@." (string_of_loc loc)))
      else Ok ()
  | Parsing.Parsed_ast.ObjField (obj_name, field_name) ->
      get_obj_class_defn obj_name env class_defns loc
      >>= fun class_defn ->
      get_class_field field_name class_defn loc
      >>= fun (TField (modifier, _, _, _)) ->
      if modifier = MConst then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Assigning expr to a const field.@."
                (string_of_loc loc)))
      else Ok ()

let check_identifier_consumable class_defns id env loc =
  let open Result in
  match id with
  | Parsing.Parsed_ast.Variable x ->
      if x = Var_name.of_string "this" then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Trying to consume 'this'.@." (string_of_loc loc)))
      else Ok ()
  | Parsing.Parsed_ast.ObjField (obj_name, field_name) ->
      get_obj_class_defn obj_name env class_defns loc
      >>= fun class_defn ->
      get_class_field field_name class_defn loc
      >>= fun (TField (modifier, _, _, _)) ->
      if modifier = MConst then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Trying to consume a const field.@."
                (string_of_loc loc)))
      else Ok ()

let check_variable_declarable var_name loc =
  if var_name = Var_name.of_string "this" then
    Error
      (Error.of_string
         (Fmt.str "%s Type error - Trying to declare 'this'.@." (string_of_loc loc)))
  else Ok ()
