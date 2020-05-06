open Core
open Ast.Ast_types
open Parsing

let rec type_generics_usage_type type_expr maybe_generic loc =
  (* in an expression we can't have nested uninitialised type parameters, unlike
     fields/function parameters where it is a form of polymorphism Foo<T> *)
  match type_expr with
  | TEInt | TEBool | TEVoid       -> Ok ()
  | TEGeneric                     -> (
    match maybe_generic with
    | Some Generic -> Ok ()
    | None         ->
        Error
          (Error.of_string
             (Fmt.str "%s Type error: Use of generic type but not in a generic class@."
                (string_of_loc loc))) )
  | TEClass (_, maybe_type_param) -> (
    match maybe_type_param with
    | Some type_param -> type_generics_usage_type type_param maybe_generic loc
    | None            -> Ok () )

let type_generics_usage_maybe_type maybe_type_expr maybe_generic loc =
  match maybe_type_expr with
  | Some type_expr -> type_generics_usage_type type_expr maybe_generic loc
  | None           -> Ok ()

let rec type_generics_usage_expr expr maybe_generic =
  let open Result in
  (* Partially apply the function for brevity in recursive calls *)
  match expr with
  | Parsed_ast.Integer _ | Parsed_ast.Boolean _ | Parsed_ast.Identifier _ -> Ok ()
  | Parsed_ast.Constructor (loc, _, maybe_type_param, constructor_args) ->
      type_generics_usage_maybe_type maybe_type_param maybe_generic loc
      >>= fun () ->
      (* Check that all the constructor arguments type-check *)
      Result.all_unit
        (List.map
           ~f:(fun (Parsed_ast.ConstructorArg (_, arg_expr)) ->
             type_generics_usage_expr arg_expr maybe_generic)
           constructor_args)
  | Parsed_ast.Let (loc, maybe_type_annot, _, bound_expr) ->
      (* Infer type of expression that is being subbed and bind it to the let var*)
      type_generics_usage_maybe_type maybe_type_annot maybe_generic loc
      >>= fun () -> type_generics_usage_expr bound_expr maybe_generic
  | Parsed_ast.Assign (_, _, assigned_expr) ->
      type_generics_usage_expr assigned_expr maybe_generic
  | Parsed_ast.Consume _ -> Ok ()
  | Parsed_ast.MethodApp (_, _, _, args_exprs) ->
      Result.all_unit
        (List.map
           ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_generic)
           args_exprs)
  | Parsed_ast.FunctionApp (_, _, args_exprs) ->
      Result.all_unit
        (List.map
           ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_generic)
           args_exprs)
  | Parsed_ast.Printf (_, _, args_exprs) ->
      Result.all_unit
        (List.map
           ~f:(fun arg_expr -> type_generics_usage_expr arg_expr maybe_generic)
           args_exprs)
  | Parsed_ast.FinishAsync (_, async_exprs, curr_thread_expr) ->
      Result.all_unit
        (List.map
           ~f:(fun (Parsed_ast.AsyncExpr async_block_expr) ->
             type_generics_usage_block_expr async_block_expr maybe_generic)
           async_exprs)
      >>= fun () -> type_generics_usage_block_expr curr_thread_expr maybe_generic
  (* We return type of the expr occurring on the current thread, not the forked thread *)
  | Parsed_ast.If (_, cond_expr, then_expr, else_expr) ->
      type_generics_usage_expr cond_expr maybe_generic
      >>= fun () ->
      type_generics_usage_block_expr then_expr maybe_generic
      >>= fun () -> type_generics_usage_block_expr else_expr maybe_generic
  | Parsed_ast.While (_, cond_expr, loop_expr) ->
      type_generics_usage_expr cond_expr maybe_generic
      >>= fun () -> type_generics_usage_block_expr loop_expr maybe_generic
  | Parsed_ast.For
      (loc, start_expr, cond_expr, step_expr, Parsed_ast.Block (block_loc, loop_expr)) ->
      (* desugar into a while loop *)
      type_generics_usage_block_expr
        (Parsed_ast.Block
           ( loc
           , [ start_expr
             ; Parsed_ast.While
                 (loc, cond_expr, Parsed_ast.Block (block_loc, loop_expr @ [step_expr]))
             ] ))
        maybe_generic
  | Parsed_ast.BinOp (_, _, expr1, expr2) ->
      type_generics_usage_expr expr1 maybe_generic
      >>= fun () -> type_generics_usage_expr expr2 maybe_generic
  | Parsed_ast.UnOp (_, _, expr) -> type_generics_usage_expr expr maybe_generic

and type_generics_usage_block_expr (Parsed_ast.Block (_, exprs)) maybe_generic =
  Result.all_unit
    (List.map ~f:(fun expr -> type_generics_usage_expr expr maybe_generic) exprs)

let rec type_generics_type_sig type_expr maybe_generic error_prefix_str =
  (* Bolt doesn't allow polymorphic types Foo<T> to be used if T is not initialised. s*)
  match type_expr with
  | TEInt | TEBool | TEVoid       -> Ok ()
  | TEGeneric                     -> (
    match maybe_generic with
    | Some Generic -> Ok ()
    | None         ->
        Error
          (Error.of_string
             (Fmt.str "Type error: Using generic type in %s but not in a generic class@."
                error_prefix_str)) )
  | TEClass (_, maybe_type_param) -> (
    match maybe_type_param with
    | Some type_param -> type_generics_type_sig type_param maybe_generic error_prefix_str
    | None            -> Ok () )

let type_generics_usage_function_defn
    (Parsed_ast.TFunction (func_name, _, return_type, params, body_expr)) =
  let open Result in
  let error_prefix_str = Fmt.str "function %s" (Function_name.to_string func_name) in
  type_generics_type_sig return_type None error_prefix_str
  >>= fun () ->
  Result.all_unit
    (List.map
       ~f:(fun (TParam (param_type, _, _, _)) ->
         type_generics_type_sig param_type None (Function_name.to_string func_name))
       params)
  >>= fun () -> type_generics_usage_block_expr body_expr None

let type_generics_usage_method_defn maybe_generic class_name
    (Parsed_ast.TMethod (meth_name, _, return_type, params, _, body_expr)) =
  let open Result in
  let error_prefix_str =
    Fmt.str "%s's method %s"
      (Class_name.to_string class_name)
      (Method_name.to_string meth_name) in
  type_generics_type_sig return_type maybe_generic error_prefix_str
  >>= fun () ->
  Result.all_unit
    (List.map
       ~f:(fun (TParam (param_type, _, _, _)) ->
         type_generics_type_sig param_type maybe_generic error_prefix_str)
       params)
  >>= fun () -> type_generics_usage_block_expr body_expr maybe_generic

let type_generics_usage_field_defn maybe_generic class_name
    (TField (_, field_type, field_name, _)) =
  let error_prefix_str =
    Fmt.str "%s's field %s"
      (Class_name.to_string class_name)
      (Field_name.to_string field_name) in
  type_generics_type_sig field_type maybe_generic error_prefix_str

let type_generics_usage_class_defn
    (Parsed_ast.TClass (class_name, maybe_generic, _, _, field_defns, method_defns)) =
  let open Result in
  Result.all_unit
    (List.map ~f:(type_generics_usage_field_defn maybe_generic class_name) field_defns)
  >>= fun () ->
  Result.all_unit
    (List.map ~f:(type_generics_usage_method_defn maybe_generic class_name) method_defns)

let instantiate_maybe_generic_this
    (Parsed_ast.TClass (class_name, maybe_generic, _, _, _, _)) =
  let maybe_type_param =
    match maybe_generic with Some Generic -> Some TEGeneric | None -> None in
  (Var_name.of_string "this", TEClass (class_name, maybe_type_param))

let rec instantiate_maybe_generic_type type_param type_expr =
  match type_expr with
  | TEGeneric -> type_param
  | TEClass (class_name, maybe_parameterised_type) ->
      ( match maybe_parameterised_type with
      | Some maybe_generic_parameterised_type ->
          Some
            (instantiate_maybe_generic_type type_param maybe_generic_parameterised_type)
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
        (class_name, maybe_generic, maybe_inherits, caps, field_defns, method_defns) as
    class_defn ) loc =
  match (maybe_generic, maybe_type_param) with
  | None, None                    -> Ok class_defn
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
           , maybe_inherits
           , caps
           , instantiated_field_defns
           , instantiated_method_defns ))
