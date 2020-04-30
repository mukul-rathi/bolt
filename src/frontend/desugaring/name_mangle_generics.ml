open Core
open Ast.Ast_types
open Typing

let name_mangle_generic_class class_name type_param =
  Class_name.of_string
    (Fmt.str "_%s%s" (Class_name.to_string class_name) (string_of_type type_param))

let name_mangle_if_generic_class class_name = function
  | Some type_param -> name_mangle_generic_class class_name type_param
  | None            -> class_name

let name_mangle_if_generic_type type_expr =
  match type_expr with
  | TEBool | TEInt | TEVoid | TEGeneric -> type_expr
  | TEClass (class_name, maybe_type_param) ->
      TEClass (name_mangle_if_generic_class class_name maybe_type_param, None)

let name_mangle_generics_usage_id id =
  match id with
  | Typed_ast.Variable (var_type, var_name) ->
      Typed_ast.Variable (name_mangle_if_generic_type var_type, var_name)
  | Typed_ast.ObjField (obj_class, maybe_type, obj_name, field_type, field_name) ->
      Typed_ast.ObjField
        ( name_mangle_if_generic_class obj_class maybe_type
        , None
        , obj_name
        , name_mangle_if_generic_type field_type
        , field_name )

let rec name_mangle_generics_usage_expr expr =
  match expr with
  | Typed_ast.Integer _ | Typed_ast.Boolean _ -> expr
  | Typed_ast.Identifier (loc, id) ->
      Typed_ast.Identifier (loc, name_mangle_generics_usage_id id)
  | Typed_ast.BlockExpr (loc, block_expr) ->
      Typed_ast.BlockExpr (loc, name_mangle_generics_usage_block_expr block_expr)
  | Typed_ast.Constructor (loc, class_name, maybe_, constructor_args) ->
      List.map
        ~f:(fun (Typed_ast.ConstructorArg (type_expr, field_name, arg_expr)) ->
          Typed_ast.ConstructorArg
            ( name_mangle_if_generic_type type_expr
            , field_name
            , name_mangle_generics_usage_expr arg_expr ))
        constructor_args
      |> fun name_mangled_args ->
      Typed_ast.Constructor
        (loc, name_mangle_if_generic_class class_name maybe_, None, name_mangled_args)
  | Typed_ast.Let (loc, type_expr, var_name, bound_expr) ->
      Typed_ast.Let
        ( loc
        , name_mangle_if_generic_type type_expr
        , var_name
        , name_mangle_generics_usage_expr bound_expr )
  | Typed_ast.Assign (loc, type_expr, id, assigned_expr) ->
      Typed_ast.Assign
        ( loc
        , name_mangle_if_generic_type type_expr
        , name_mangle_generics_usage_id id
        , name_mangle_generics_usage_expr assigned_expr )
  | Typed_ast.Consume (loc, id) ->
      Typed_ast.Consume (loc, name_mangle_generics_usage_id id)
  | Typed_ast.MethodApp
      (loc, return_type, param_types, obj_name, obj_class, maybe_, meth_name, args) ->
      List.map ~f:name_mangle_if_generic_type param_types
      |> fun name_mangled_param_types ->
      List.map ~f:name_mangle_generics_usage_expr args
      |> fun name_mangled_args ->
      Typed_ast.MethodApp
        ( loc
        , name_mangle_if_generic_type return_type
        , name_mangled_param_types
        , obj_name
        , name_mangle_if_generic_class obj_class maybe_
        , None
        , meth_name
        , name_mangled_args )
  | Typed_ast.FunctionApp (loc, return_type, param_types, func_name, args) ->
      List.map ~f:name_mangle_if_generic_type param_types
      |> fun name_mangled_param_types ->
      List.map ~f:name_mangle_generics_usage_expr args
      |> fun name_mangled_args ->
      Typed_ast.FunctionApp
        ( loc
        , name_mangle_if_generic_type return_type
        , name_mangled_param_types
        , func_name
        , name_mangled_args )
  | Typed_ast.Printf (loc, format_str, args) ->
      List.map ~f:name_mangle_generics_usage_expr args
      |> fun name_mangled_args -> Typed_ast.Printf (loc, format_str, name_mangled_args)
  | Typed_ast.FinishAsync (loc, type_expr, async_exprs, curr_thread_expr) ->
      List.map
        ~f:(fun (Typed_ast.AsyncExpr async_expr) ->
          Typed_ast.AsyncExpr (name_mangle_generics_usage_block_expr async_expr))
        async_exprs
      |> fun name_mangled_async_exprs ->
      Typed_ast.FinishAsync
        ( loc
        , name_mangle_if_generic_type type_expr
        , name_mangled_async_exprs
        , name_mangle_generics_usage_block_expr curr_thread_expr )
  | Typed_ast.If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      Typed_ast.If
        ( loc
        , name_mangle_if_generic_type type_expr
        , name_mangle_generics_usage_expr cond_expr
        , name_mangle_generics_usage_block_expr then_expr
        , name_mangle_generics_usage_block_expr else_expr )
  | Typed_ast.While (loc, cond_expr, loop_expr) ->
      Typed_ast.While
        ( loc
        , name_mangle_generics_usage_expr cond_expr
        , name_mangle_generics_usage_block_expr loop_expr )
  | Typed_ast.BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      Typed_ast.BinOp
        ( loc
        , name_mangle_if_generic_type type_expr
        , bin_op
        , name_mangle_generics_usage_expr expr1
        , name_mangle_generics_usage_expr expr2 )
  | Typed_ast.UnOp (loc, type_expr, un_op, op_expr) ->
      Typed_ast.UnOp
        ( loc
        , name_mangle_if_generic_type type_expr
        , un_op
        , name_mangle_generics_usage_expr op_expr )

and name_mangle_generics_usage_block_expr (Typed_ast.Block (loc, type_expr, exprs)) =
  List.map ~f:name_mangle_generics_usage_expr exprs
  |> fun name_mangled_exprs ->
  Typed_ast.Block (loc, name_mangle_if_generic_type type_expr, name_mangled_exprs)

let name_mangle_generics_usage_field_defn
    (TField (modifier, field_type, field_name, field_caps)) =
  TField (modifier, name_mangle_if_generic_type field_type, field_name, field_caps)

let name_mangle_generics_usage_param
    (TParam (param_type, param_name, optional_caps, maybe_borrowed)) =
  TParam
    (name_mangle_if_generic_type param_type, param_name, optional_caps, maybe_borrowed)

let instantiate_maybe_generic_method_defn
    (Typed_ast.TMethod
      (meth_name, maybe_borrowed_ref_ret, return_type, params, meth_cap_names, body_expr))
    =
  List.map ~f:name_mangle_generics_usage_param params
  |> fun name_mangled_params ->
  Typed_ast.TMethod
    ( meth_name
    , maybe_borrowed_ref_ret
    , name_mangle_if_generic_type return_type
    , name_mangled_params
    , meth_cap_names
    , name_mangle_generics_usage_block_expr body_expr )

let name_mangle_generics_usage_class_defn
    (Typed_ast.TClass (class_name, _, maybe_inherits, caps, field_defns, method_defns)) =
  List.map ~f:name_mangle_generics_usage_field_defn field_defns
  |> fun name_mangled_field_defns ->
  List.map ~f:instantiate_maybe_generic_method_defn method_defns
  |> fun name_mangled_method_defns ->
  Typed_ast.TClass
    ( class_name
    , None
    , maybe_inherits
    , caps
    , name_mangled_field_defns
    , name_mangled_method_defns )

let name_mangle_generics_usage_function_defn
    (Typed_ast.TFunction
      (func_name, maybe_borrowed_ref_ret, return_type, params, body_expr)) =
  List.map ~f:name_mangle_generics_usage_param params
  |> fun name_mangled_params ->
  Typed_ast.TFunction
    ( func_name
    , maybe_borrowed_ref_ret
    , name_mangle_if_generic_type return_type
    , name_mangled_params
    , name_mangle_generics_usage_block_expr body_expr )

let name_mangle_generics_usage_program
    (Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  List.map ~f:name_mangle_generics_usage_class_defn class_defns
  |> fun name_mangled_class_defns ->
  List.map ~f:name_mangle_generics_usage_function_defn function_defns
  |> fun name_mangled_function_defns ->
  Typed_ast.Prog
    ( name_mangled_class_defns
    , name_mangled_function_defns
    , name_mangle_generics_usage_block_expr main_expr )
