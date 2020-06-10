open Ast.Ast_types
open Core
open Type_generics
open Parsing

type type_binding = Var_name.t * type_expr
type type_env = type_binding list

(********** GETTER METHODS for type-checking core language *********)

let rec get_var_type (var_name : Var_name.t) (env : type_env) loc =
  match env with
  | []                            ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Variable %s not defined in environment@."
              (string_of_loc loc) (Var_name.to_string var_name)))
  | (var_name', var_type) :: env' ->
      if var_name' = var_name then Ok var_type else get_var_type var_name env' loc

let get_class_defn class_name class_defns loc =
  let matching_class_defns =
    List.filter
      ~f:(fun (Parsed_ast.TClass (name, _, _, _, _, _)) -> class_name = name)
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

let get_instantiated_class_defn class_name class_defns maybe_type_param loc =
  let open Result in
  get_class_defn class_name class_defns loc
  >>= fun maybe_uninstantiated_class_defn ->
  instantiate_maybe_generic_class_defn maybe_type_param maybe_uninstantiated_class_defn
    loc

let rec get_class_capabilities class_name class_defns =
  let open Result in
  get_class_defn class_name class_defns Lexing.dummy_pos
  >>= fun (Parsed_ast.TClass (_, _, maybe_inherits, capabilities, _, _)) ->
  ( match maybe_inherits with
  | Some super_class -> get_class_capabilities super_class class_defns
  | None             -> Ok [] )
  >>| fun superclass_caps -> List.concat [superclass_caps; capabilities]

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

let rec get_class_field field_name class_defns
    (Parsed_ast.TClass (_, _, maybe_inherits, _, field_defns, _)) maybe_type_param loc =
  let open Result in
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  match matching_field_defns with
  | []      -> (
    match maybe_inherits with
    | Some superclass ->
        get_instantiated_class_defn superclass class_defns maybe_type_param loc
        >>= fun superclass_defn ->
        get_class_field field_name class_defns superclass_defn maybe_type_param loc
    | None            ->
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Field %s not defined in environment@."
                (string_of_loc loc)
                (Field_name.to_string field_name))) )
  | [field] -> Ok field
  | _       ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - Field %s has duplicate definitions in environment@."
              (string_of_loc loc)
              (Field_name.to_string field_name)))

let rec get_class_methods class_defns
    (Parsed_ast.TClass (_, _, maybe_inherits, _, _, method_defns)) maybe_type_param loc =
  let open Result in
  ( match maybe_inherits with
  | Some superclass ->
      ( match maybe_type_param with
      (* if we can instantiate then get instantiated class definition. *)
      | None -> get_class_defn superclass class_defns loc
      | _ -> get_instantiated_class_defn superclass class_defns maybe_type_param loc )
      >>= fun superclass_defn ->
      get_class_methods class_defns superclass_defn maybe_type_param loc
  | None            -> Ok [] )
  >>| fun superclass_methods ->
  List.concat [superclass_methods; method_defns]
  (* filter out overriden methods (i.e those with same name and params) *)
  |> List.dedup_and_sort
       ~compare:(fun (Parsed_ast.TMethod (name_1, _, _, params_1, _, _))
                     (Parsed_ast.TMethod (name_2, _, _, params_2, _, _))
                     ->
         if name_1 = name_2 && get_params_types params_1 = get_params_types params_2 then
           0
         else 1)

let get_obj_class_defn var_name env class_defns loc =
  let open Result in
  get_var_type var_name env loc
  >>= function
  | TEClass (class_name, maybe_type_param) ->
      get_instantiated_class_defn class_name class_defns maybe_type_param loc
      >>| fun instantiated_class_defn -> (instantiated_class_defn, maybe_type_param)
  | wrong_type ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - %s should be an object, instead is of type %s@."
              (string_of_loc loc) (Var_name.to_string var_name)
              (string_of_type wrong_type)))

(********** CHECK METHODS for checking invariants *********)

let check_no_duplicate_var_declarations_in_block exprs loc =
  if
    List.contains_dup
      ~compare:(fun expr1 expr2 ->
        match expr1 with
        | Parsed_ast.Let (_, _, var_name1, _) -> (
          match expr2 with
          | Parsed_ast.Let (_, _, var_name2, _) ->
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
  | Parsed_ast.Variable x ->
      if x = Var_name.of_string "this" then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Assigning expr to 'this'.@." (string_of_loc loc)))
      else Ok ()
  | Parsed_ast.ObjField (obj_name, field_name) ->
      get_obj_class_defn obj_name env class_defns loc
      >>= fun (class_defn, maybe_type_param) ->
      get_class_field field_name class_defns class_defn maybe_type_param loc
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
  | Parsed_ast.Variable x ->
      if x = Var_name.of_string "this" then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Trying to consume 'this'.@." (string_of_loc loc)))
      else Ok ()
  | Parsed_ast.ObjField (obj_name, field_name) ->
      get_obj_class_defn obj_name env class_defns loc
      >>= fun (class_defn, maybe_type_param) ->
      get_class_field field_name class_defns class_defn maybe_type_param loc
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
