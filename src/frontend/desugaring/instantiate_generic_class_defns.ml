open Ast.Ast_types
open Name_mangle_generics
open Typing
open Core

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

let instantiate_maybe_generic_maybe_type type_param maybe_type_expr =
  match maybe_type_expr with
  | Some type_expr -> Some (instantiate_maybe_generic_type type_param type_expr)
  | None           -> None

let instantiate_maybe_generic_id type_param id =
  match id with
  | Typed_ast.Variable (var_type, var_name) ->
      Typed_ast.Variable (instantiate_maybe_generic_type type_param var_type, var_name)
  | Typed_ast.ObjField (obj_class, maybe_type, obj_name, field_type, field_name) ->
      Typed_ast.ObjField
        ( obj_class
        , instantiate_maybe_generic_maybe_type type_param maybe_type
        , obj_name
        , instantiate_maybe_generic_type type_param field_type
        , field_name )

let rec instantiate_maybe_generic_expr type_param expr =
  match expr with
  | Typed_ast.Integer _ | Typed_ast.Boolean _ -> expr
  | Typed_ast.Identifier (loc, id) ->
      Typed_ast.Identifier (loc, instantiate_maybe_generic_id type_param id)
  | Typed_ast.BlockExpr (loc, block_expr) ->
      Typed_ast.BlockExpr (loc, instantiate_maybe_generic_block_expr type_param block_expr)
  | Typed_ast.Constructor (loc, class_name, maybe_type_param, constructor_args) ->
      List.map
        ~f:(fun (Typed_ast.ConstructorArg (type_expr, field_name, arg_expr)) ->
          Typed_ast.ConstructorArg
            ( instantiate_maybe_generic_type type_param type_expr
            , field_name
            , instantiate_maybe_generic_expr type_param arg_expr ))
        constructor_args
      |> fun instantiated_args ->
      Typed_ast.Constructor
        ( loc
        , class_name
        , instantiate_maybe_generic_maybe_type type_param maybe_type_param
        , instantiated_args )
  | Typed_ast.Let (loc, type_expr, var_name, bound_expr) ->
      Typed_ast.Let
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , var_name
        , instantiate_maybe_generic_expr type_param bound_expr )
  | Typed_ast.Assign (loc, type_expr, id, assigned_expr) ->
      Typed_ast.Assign
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , instantiate_maybe_generic_id type_param id
        , instantiate_maybe_generic_expr type_param assigned_expr )
  | Typed_ast.Consume (loc, id) ->
      Typed_ast.Consume (loc, instantiate_maybe_generic_id type_param id)
  | Typed_ast.MethodApp
      (loc, return_type, param_types, obj_name, obj_class, maybe_type, meth_name, args) ->
      List.map ~f:(instantiate_maybe_generic_type type_param) param_types
      |> fun instantiated_param_types ->
      List.map ~f:(instantiate_maybe_generic_expr type_param) args
      |> fun instantiated_args ->
      Typed_ast.MethodApp
        ( loc
        , instantiate_maybe_generic_type type_param return_type
        , instantiated_param_types
        , obj_name
        , obj_class
        , instantiate_maybe_generic_maybe_type type_param maybe_type
        , meth_name
        , instantiated_args )
  | Typed_ast.FunctionApp (loc, return_type, param_types, func_name, args) ->
      List.map ~f:(instantiate_maybe_generic_type type_param) param_types
      |> fun instantiated_param_types ->
      List.map ~f:(instantiate_maybe_generic_expr type_param) args
      |> fun instantiated_args ->
      Typed_ast.FunctionApp
        ( loc
        , instantiate_maybe_generic_type type_param return_type
        , instantiated_param_types
        , func_name
        , instantiated_args )
  | Typed_ast.Printf (loc, format_str, args) ->
      List.map ~f:(instantiate_maybe_generic_expr type_param) args
      |> fun instantiated_args -> Typed_ast.Printf (loc, format_str, instantiated_args)
  | Typed_ast.FinishAsync (loc, type_expr, async_exprs, curr_thread_expr) ->
      List.map
        ~f:(fun (Typed_ast.AsyncExpr async_expr) ->
          Typed_ast.AsyncExpr (instantiate_maybe_generic_block_expr type_param async_expr))
        async_exprs
      |> fun instantiated_async_exprs ->
      Typed_ast.FinishAsync
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , instantiated_async_exprs
        , instantiate_maybe_generic_block_expr type_param curr_thread_expr )
  | Typed_ast.If (loc, type_expr, cond_expr, then_expr, else_expr) ->
      Typed_ast.If
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , instantiate_maybe_generic_expr type_param cond_expr
        , instantiate_maybe_generic_block_expr type_param then_expr
        , instantiate_maybe_generic_block_expr type_param else_expr )
  | Typed_ast.While (loc, cond_expr, loop_expr) ->
      Typed_ast.While
        ( loc
        , instantiate_maybe_generic_expr type_param cond_expr
        , instantiate_maybe_generic_block_expr type_param loop_expr )
  | Typed_ast.BinOp (loc, type_expr, bin_op, expr1, expr2) ->
      Typed_ast.BinOp
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , bin_op
        , instantiate_maybe_generic_expr type_param expr1
        , instantiate_maybe_generic_expr type_param expr2 )
  | Typed_ast.UnOp (loc, type_expr, un_op, op_expr) ->
      Typed_ast.UnOp
        ( loc
        , instantiate_maybe_generic_type type_param type_expr
        , un_op
        , instantiate_maybe_generic_expr type_param op_expr )

and instantiate_maybe_generic_block_expr type_param
    (Typed_ast.Block (loc, type_expr, exprs)) =
  List.map ~f:(instantiate_maybe_generic_expr type_param) exprs
  |> fun instantiated_exprs ->
  Typed_ast.Block
    (loc, instantiate_maybe_generic_type type_param type_expr, instantiated_exprs)

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
    (Typed_ast.TMethod
      (meth_name, maybe_borrowed_ref_ret, return_type, params, meth_cap_names, body_expr))
    =
  List.map ~f:(instantiate_maybe_generic_param type_param) params
  |> fun instantiated_params ->
  Typed_ast.TMethod
    ( meth_name
    , maybe_borrowed_ref_ret
    , instantiate_maybe_generic_type type_param return_type
    , instantiated_params
    , meth_cap_names
    , instantiate_maybe_generic_block_expr type_param body_expr )

let instantiate_generic_class_defn type_params
    (Typed_ast.TClass (class_name, _, maybe_inherits, caps, field_defns, method_defns)) =
  List.map
    ~f:(fun type_param ->
      List.map ~f:(instantiate_maybe_generic_field_defn type_param) field_defns
      |> fun instantiated_field_defns ->
      List.map ~f:(instantiate_maybe_generic_method_defn type_param) method_defns
      |> fun instantiated_method_defns ->
      ( match maybe_inherits with
      | Some superclass -> Some (name_mangle_generic_class superclass type_param)
      | None            -> None )
      |> fun name_mangled_maybe_inherits ->
      Typed_ast.TClass
        ( name_mangle_generic_class class_name type_param
        , None
        , name_mangled_maybe_inherits
        , caps
        , instantiated_field_defns
        , instantiated_method_defns ))
    type_params

let instantiate_generic_class_defns class_defns class_insts =
  List.fold ~init:class_defns
    ~f:(fun acc_class_defns (class_name, type_params) ->
      (* replace each generic class with its concrete instantiations *)
      List.find_exn
        ~f:(fun (Typed_ast.TClass (name, _, _, _, _, _)) -> name = class_name)
        acc_class_defns
      |> fun class_defn ->
      instantiate_generic_class_defn type_params class_defn
      |> fun instantiated_class_defns ->
      List.filter
        ~f:(fun (Typed_ast.TClass (name, _, _, _, _, _)) -> not (name = class_name))
        acc_class_defns
      |> fun other_class_defns -> List.concat [instantiated_class_defns; other_class_defns])
    class_insts
  |> (* get rid of uninitialised generic classes *)
  List.filter ~f:(fun (Typed_ast.TClass (_, maybe_generic, _, _, _, _)) ->
      match maybe_generic with Some Generic -> false | None -> true)
