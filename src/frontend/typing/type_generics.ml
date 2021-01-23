open Core
open Ast.Ast_types
open Parsing

let rec type_generics_usage_type type_expr maybe_in_generic_class error_prefix_str =
  match maybe_in_generic_class with
  | Some Generic -> Ok () (* can have generics in generic class *)
  | None         -> (
    (* recursively check there aren't nested uninitialised type parameters *)
    match type_expr with
    | TEInt | TEBool | TEVoid       -> Ok ()
    | TEGeneric                     ->
        Error
          (Error.of_string
             (Fmt.str "%s Type error: Use of generic type but not in a generic class@."
                error_prefix_str))
    | TEClass (_, maybe_type_param) -> (
      match maybe_type_param with
      | Some type_param ->
          type_generics_usage_type type_param maybe_in_generic_class error_prefix_str
      | None            -> Ok () ) )

let type_generics_usage_maybe_type maybe_type_expr maybe_in_generic_class error_prefix_str
    =
  match maybe_type_expr with
  | Some type_expr ->
      type_generics_usage_type type_expr maybe_in_generic_class error_prefix_str
  | None           -> Ok ()

let rec type_generics_usage_expr expr maybe_in_generic_class =
  let open Result in
  match maybe_in_generic_class with
  | Some Generic -> Ok () (* can have generics in generic class *)
  | None         -> (
    match expr with
    | Parsed_ast.Integer _ | Parsed_ast.Boolean _ | Parsed_ast.Identifier _ -> Ok ()
    | Parsed_ast.Constructor (loc, _, maybe_type_param, constructor_args) ->
        type_generics_usage_maybe_type maybe_type_param maybe_in_generic_class
          (string_of_loc loc)
        >>= fun () ->
        (* Check that all the constructor arguments type-check *)
        Result.all_unit
          (List.map
             ~f:(fun (Parsed_ast.ConstructorArg (_, arg_expr)) ->
               type_generics_usage_expr arg_expr maybe_in_generic_class)
             constructor_args)
    | Parsed_ast.Let (loc, maybe_type_annot, _, bound_expr) ->
        type_generics_usage_maybe_type maybe_type_annot maybe_in_generic_class
          (string_of_loc loc)
        >>= fun () -> type_generics_usage_expr bound_expr maybe_in_generic_class
    | Parsed_ast.Assign (_, _, assigned_expr) ->
        type_generics_usage_expr assigned_expr maybe_in_generic_class
    | Parsed_ast.Consume _ -> Ok ()
    | Parsed_ast.MethodApp (_, _, _, args_exprs) ->
        Result.all_unit
          (List.map
             ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_in_generic_class)
             args_exprs)
    | Parsed_ast.FunctionApp (_, _, args_exprs) ->
        Result.all_unit
          (List.map
             ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_in_generic_class)
             args_exprs)
    | Parsed_ast.Printf (_, _, args_exprs) ->
        Result.all_unit
          (List.map
             ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_in_generic_class)
             args_exprs)
    | Parsed_ast.FinishAsync (_, async_exprs, curr_thread_expr) ->
        Result.all_unit
          (List.map
             ~f:(fun (Parsed_ast.AsyncExpr async_block_expr) ->
               type_generics_usage_block_expr async_block_expr maybe_in_generic_class)
             async_exprs)
        >>= fun () ->
        type_generics_usage_block_expr curr_thread_expr maybe_in_generic_class
    | Parsed_ast.If (_, cond_expr, then_expr, else_expr) ->
        type_generics_usage_expr cond_expr maybe_in_generic_class
        >>= fun () ->
        type_generics_usage_block_expr then_expr maybe_in_generic_class
        >>= fun () -> type_generics_usage_block_expr else_expr maybe_in_generic_class
    | Parsed_ast.While (_, cond_expr, loop_expr) ->
        type_generics_usage_expr cond_expr maybe_in_generic_class
        >>= fun () -> type_generics_usage_block_expr loop_expr maybe_in_generic_class
    | Parsed_ast.For (_, start_expr, cond_expr, step_expr, loop_expr) ->
        type_generics_usage_expr start_expr maybe_in_generic_class
        >>= fun () ->
        type_generics_usage_expr cond_expr maybe_in_generic_class
        >>= fun () ->
        type_generics_usage_expr step_expr maybe_in_generic_class
        >>= fun () -> type_generics_usage_block_expr loop_expr maybe_in_generic_class
    | Parsed_ast.BinOp (_, _, expr1, expr2) ->
        type_generics_usage_expr expr1 maybe_in_generic_class
        >>= fun () -> type_generics_usage_expr expr2 maybe_in_generic_class
    | Parsed_ast.UnOp (_, _, expr) -> type_generics_usage_expr expr maybe_in_generic_class
    )

and type_generics_usage_block_expr (Parsed_ast.Block (_, exprs)) maybe_in_generic_class =
  Result.all_unit
    (List.map ~f:(fun expr -> type_generics_usage_expr expr maybe_in_generic_class) exprs)

let type_generics_type_sig return_type params maybe_in_generic_class error_prefix_str =
  let open Result in
  type_generics_usage_type return_type maybe_in_generic_class
    (Fmt.str "%s return type -" error_prefix_str)
  >>= fun () ->
  Result.all_unit
    (List.map
       ~f:(fun (TParam (param_type, param_name, _, _)) ->
         type_generics_usage_type param_type maybe_in_generic_class
           (Fmt.str "%s param %s type -" error_prefix_str (Var_name.to_string param_name)))
       params)

let type_generics_usage_function_defn
    (Parsed_ast.TFunction (func_name, _, return_type, params, body_expr)) =
  let open Result in
  let error_prefix_str = Fmt.str "Function %s:" (Function_name.to_string func_name) in
  type_generics_type_sig return_type params None error_prefix_str
  >>= fun () -> type_generics_usage_block_expr body_expr None

let type_generics_usage_method_defn maybe_in_generic_class class_name
    (Parsed_ast.TMethod (meth_name, _, return_type, params, _, body_expr)) =
  let open Result in
  let error_prefix_str =
    Fmt.str "%s's method %s:"
      (Class_name.to_string class_name)
      (Method_name.to_string meth_name) in
  type_generics_type_sig return_type params maybe_in_generic_class error_prefix_str
  >>= fun () -> type_generics_usage_block_expr body_expr maybe_in_generic_class

let type_generics_usage_field_defn maybe_in_generic_class class_name
    (TField (_, field_type, field_name, _)) =
  let error_prefix_str =
    Fmt.str "%s's field %s"
      (Class_name.to_string class_name)
      (Field_name.to_string field_name) in
  type_generics_usage_type field_type maybe_in_generic_class error_prefix_str

let type_generics_usage_class_defn
    (Parsed_ast.TClass
      (class_name, maybe_in_generic_class, _, _, field_defns, method_defns)) =
  let open Result in
  match maybe_in_generic_class with
  | Some Generic -> Ok ()
  | None         ->
      Result.all_unit
        (List.map
           ~f:(type_generics_usage_field_defn maybe_in_generic_class class_name)
           field_defns)
      >>= fun () ->
      Result.all_unit
        (List.map
           ~f:(type_generics_usage_method_defn maybe_in_generic_class class_name)
           method_defns)

let type_generics_usage_main_expr main_expr =
  type_generics_usage_block_expr main_expr None

(*****************)
(* Instantiating generics with concrete types *)
(*****************)

let instantiate_maybe_generic_this
    (Parsed_ast.TClass (class_name, maybe_in_generic_class, _, _, _, _)) =
  let maybe_type_param =
    (* use generic type T inside class *)
    match maybe_in_generic_class with Some Generic -> Some TEGeneric | None -> None in
  (Var_name.of_string "this", TEClass (class_name, maybe_type_param))

let rec instantiate_maybe_generic_type type_param type_expr =
  match type_expr with
  | TEGeneric -> type_param
  | TEClass (class_name, maybe_parameterised_type) ->
      ( match maybe_parameterised_type with
      | Some maybe_in_generic_class_parameterised_type ->
          Some
            (instantiate_maybe_generic_type type_param
               maybe_in_generic_class_parameterised_type)
      | None -> None )
      |> fun updated_parameterised_type -> TEClass (class_name, updated_parameterised_type)
  | TEBool | TEInt | TEVoid -> type_expr

let instantiate_maybe_generic_field_defn type_param
    (TField (modifier, field_type, field_name, field_caps)) =
  TField
    ( modifier
    , instantiate_maybe_generic_type type_param field_type
    , field_name
    , field_caps )

let instantiate_maybe_generic_param type_param
    (TParam (param_type, param_name, optional_caps, maybe_borrowed)) =
  TParam
    ( instantiate_maybe_generic_type type_param param_type
    , param_name
    , optional_caps
    , maybe_borrowed )

let instantiate_maybe_generic_method_defn type_param
    (Parsed_ast.TMethod
      (meth_name, maybe_borrowed_ref_ret, return_type, params, meth_cap_names, body_expr))
    =
  List.map ~f:(instantiate_maybe_generic_param type_param) params
  |> fun instantiated_params ->
  Parsed_ast.TMethod
    ( meth_name
    , maybe_borrowed_ref_ret
    , instantiate_maybe_generic_type return_type type_param
    , instantiated_params
    , meth_cap_names
    , body_expr )

let instantiate_maybe_generic_class_defn maybe_type_param
    ( Parsed_ast.TClass
        (class_name, maybe_generic, maybe_superclass, caps, field_defns, method_defns) as
    class_defn ) loc =
  match (maybe_generic, maybe_type_param) with
  | None, None                    -> (* not a generic class *) Ok class_defn
  | None, Some type_param         ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - non-generic class %s is being instantiated with a type parameter %s@."
              (string_of_loc loc)
              (Class_name.to_string class_name)
              (string_of_type type_param)))
  | Some Generic, None            ->
      Error
        (Error.of_string
           (Fmt.str
              "%s Type error - generic class %s needs to be instantiated with a type parameter@."
              (string_of_loc loc)
              (Class_name.to_string class_name)))
  | Some Generic, Some type_param ->
      List.map ~f:(instantiate_maybe_generic_field_defn type_param) field_defns
      |> fun instantiated_field_defns ->
      List.map ~f:(instantiate_maybe_generic_method_defn type_param) method_defns
      |> fun instantiated_method_defns ->
      Ok
        (Parsed_ast.TClass
           ( class_name
           , maybe_generic
           , maybe_superclass
           , caps
           , instantiated_field_defns
           , instantiated_method_defns ))
