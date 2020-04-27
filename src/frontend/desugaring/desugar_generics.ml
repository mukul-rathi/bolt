open Core
open Ast.Ast_types
open Desugar_env
open Typing

let name_mangle_generic_class class_name type_param =
  Class_name.of_string
    (Fmt.str "_%s%s" (Class_name.to_string class_name) (string_of_type type_param))

let name_mangle_if_generic_class class_name = function
  | Some type_param -> name_mangle_generic_class class_name type_param
  | None            -> class_name

(* Count instantiations of type parameters for generic classes and polymorphic functions *)

let rec add_instantiation concrete_type elem_name = function
  | [] -> [(elem_name, [concrete_type])]
  | (name, concrete_types) :: insts ->
      if elem_name = name then
        if elem_in_list concrete_type concrete_types then (name, concrete_types) :: insts
        else (name, concrete_type :: concrete_types) :: insts
      else (name, concrete_types) :: add_instantiation concrete_type elem_name insts

let rec count_instantiations_expr expr class_insts func_insts =
  match expr with
  | Typed_ast.Integer _ | Typed_ast.Boolean _ | Typed_ast.Identifier _ ->
      (class_insts, func_insts)
  | Typed_ast.BlockExpr (_, block_expr) ->
      count_instantiations_block_expr block_expr class_insts func_insts
  | Typed_ast.Constructor (_, class_name, maybe_type_param, constructor_args) ->
      ( match maybe_type_param with
      | Some type_param -> add_instantiation type_param class_name class_insts
      | None            -> class_insts )
      |> fun updated_class_insts ->
      List.fold
        ~init:(updated_class_insts, func_insts)
        ~f:
          (fun (acc_class_insts, acc_func_insts)
               (Typed_ast.ConstructorArg (_, _, arg_expr)) ->
          count_instantiations_expr arg_expr acc_class_insts acc_func_insts)
        constructor_args
  | Typed_ast.Let (_, _, _, bound_expr) ->
      count_instantiations_expr bound_expr class_insts func_insts
  | Typed_ast.Assign (_, _, _, assigned_expr) ->
      count_instantiations_expr assigned_expr class_insts func_insts
  | Typed_ast.Consume _ -> (class_insts, func_insts)
  | Typed_ast.MethodApp (_, _, _, _, _, _, args) ->
      List.fold ~init:(class_insts, func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) arg_expr ->
          count_instantiations_expr arg_expr acc_class_insts acc_func_insts)
        args
  | Typed_ast.FunctionApp (_, _, func_params, func_name, args) ->
      add_instantiation func_params func_name func_insts
      |> fun updated_func_insts ->
      List.fold
        ~init:(class_insts, updated_func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) arg_expr ->
          count_instantiations_expr arg_expr acc_class_insts acc_func_insts)
        args
  | Typed_ast.Printf (_, _, args) ->
      List.fold ~init:(class_insts, func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) arg_expr ->
          count_instantiations_expr arg_expr acc_class_insts acc_func_insts)
        args
  | Typed_ast.FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      count_instantiations_block_expr curr_thread_expr class_insts func_insts
      |> fun (updated_class_insts, updated_func_insts) ->
      List.fold
        ~init:(updated_class_insts, updated_func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) (Typed_ast.AsyncExpr async_expr) ->
          count_instantiations_block_expr async_expr acc_class_insts acc_func_insts)
        async_exprs
  | Typed_ast.If (_, _, cond_expr, then_expr, else_expr) ->
      count_instantiations_expr cond_expr class_insts func_insts
      |> fun (updated_class_insts, updated_func_insts) ->
      List.fold
        ~init:(updated_class_insts, updated_func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) branch_expr ->
          count_instantiations_block_expr branch_expr acc_class_insts acc_func_insts)
        [then_expr; else_expr]
  | Typed_ast.While (_, cond_expr, loop_expr) ->
      count_instantiations_expr cond_expr class_insts func_insts
      |> fun (updated_class_insts, updated_func_insts) ->
      count_instantiations_block_expr loop_expr updated_class_insts updated_func_insts
  | Typed_ast.BinOp (_, _, _, expr1, expr2) ->
      List.fold ~init:(class_insts, func_insts)
        ~f:(fun (acc_class_insts, acc_func_insts) op_expr ->
          count_instantiations_expr op_expr acc_class_insts acc_func_insts)
        [expr1; expr2]
  | Typed_ast.UnOp (_, _, _, op_expr) ->
      count_instantiations_expr op_expr class_insts func_insts

and count_instantiations_block_expr (Typed_ast.Block (_, _, exprs)) class_insts func_insts
    =
  List.fold ~init:(class_insts, func_insts)
    ~f:(fun (acc_class_insts, acc_func_insts) expr ->
      count_instantiations_expr expr acc_class_insts acc_func_insts)
    exprs

let count_instantiations_function_defn (Typed_ast.TFunction (_, _, _, _, body_expr))
    class_insts func_insts =
  count_instantiations_block_expr body_expr class_insts func_insts

let count_instantiations_class_defn (Typed_ast.TClass (_, _, _, _, method_defns))
    class_insts func_insts =
  List.fold ~init:(class_insts, func_insts)
    ~f:
      (fun (acc_class_insts, acc_func_insts)
           (Typed_ast.TMethod (_, _, _, _, _, body_expr)) ->
      count_instantiations_block_expr body_expr acc_class_insts acc_func_insts)
    method_defns

let count_instantiations_program (Typed_ast.Prog (class_defns, function_defns, main_expr))
    =
  count_instantiations_block_expr main_expr [] []
  |> fun (class_insts, func_insts) ->
  List.fold ~init:(class_insts, func_insts)
    ~f:(fun (acc_class_insts, acc_func_insts) class_defn ->
      count_instantiations_class_defn class_defn acc_class_insts acc_func_insts)
    class_defns
  |> fun (updated_class_ints, updated_func_insts) ->
  List.fold
    ~init:(updated_class_ints, updated_func_insts)
    ~f:(fun (acc_class_insts, acc_func_insts) function_defn ->
      count_instantiations_function_defn function_defn acc_class_insts acc_func_insts)
    function_defns

(* NEXT, instantiate classes with concrete types *)

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
    (Typed_ast.TMethod
      (meth_name, maybe_borrowed_ref_ret, return_type, params, meth_cap_names, body_expr))
    =
  List.map ~f:(instantiate_maybe_generic_param type_param) params
  |> fun instantiated_params ->
  Typed_ast.TMethod
    ( meth_name
    , maybe_borrowed_ref_ret
    , instantiate_maybe_generic_type return_type type_param
    , instantiated_params
    , meth_cap_names
    , body_expr )

let instantiate_generic_class_defn type_params
    (Typed_ast.TClass (class_name, _, caps, field_defns, method_defns)) =
  List.map
    ~f:(fun type_param ->
      List.map ~f:(instantiate_maybe_generic_field_defn type_param) field_defns
      |> fun instantiated_field_defns ->
      List.map ~f:(instantiate_maybe_generic_method_defn type_param) method_defns
      |> fun instantiated_method_defns ->
      Typed_ast.TClass
        ( name_mangle_generic_class class_name type_param
        , None
        , caps
        , instantiated_field_defns
        , instantiated_method_defns ))
    type_params

let instantiate_all_generic_class_defns class_defns class_insts =
  List.fold ~init:class_defns
    ~f:(fun acc_class_defns (class_name, type_params) ->
      (* replace each generic class with its concrete instantiations *)
      List.find_exn
        ~f:(fun (Typed_ast.TClass (name, _, _, _, _)) -> name = class_name)
        acc_class_defns
      |> fun class_defn ->
      instantiate_generic_class_defn type_params class_defn
      |> fun instantiated_class_defns ->
      List.filter
        ~f:(fun (Typed_ast.TClass (name, _, _, _, _)) -> not (name = class_name))
        acc_class_defns
      |> fun other_class_defns -> List.concat [instantiated_class_defns; other_class_defns])
    class_insts
  |> List.filter ~f:(fun (Typed_ast.TClass (_, maybe_generic, _, _, _)) ->
         match maybe_generic with Some Generic -> false | None -> true)

(* get rid of uninitialised generic classes *)

let rec is_type_polymorphic = function
  | TEGeneric                     -> true
  | TEBool | TEInt | TEVoid       -> false
  | TEClass (_, maybe_type_param) -> (
    match maybe_type_param with
    | Some type_param -> is_type_polymorphic type_param
    | None            -> false )

let params_are_polymorphic params =
  List.exists
    ~f:(fun (TParam (param_type, _, _, _)) -> is_type_polymorphic param_type)
    params

let do_args_instantiate_params arg_types params =
  List.length arg_types = List.length params
  && List.for_all2_exn
       ~f:(fun arg_type (TParam (param_type, _, _, _)) ->
         Type_env.is_subtype_of arg_type param_type)
       arg_types params

let name_mangle_arg_type arg_type =
  match arg_type with
  | TEBool | TEInt | TEVoid | TEGeneric -> arg_type
  | TEClass (class_name, maybe_type_param) ->
      TEClass (name_mangle_if_generic_class class_name maybe_type_param, None)

let instantiate_params_with_args params arg_types =
  List.map2_exn
    ~f:(fun arg_type (TParam (_, param_name, maybe_caps, maybe_borrowed)) ->
      TParam (name_mangle_arg_type arg_type, param_name, maybe_caps, maybe_borrowed))
    arg_types params

let instantiate_function_defn_with_args func_name arg_types function_defns =
  (* If there's a polymorphic function that can be instantiated with these args,
     instantiate it *)
  List.find
    ~f:(fun (Typed_ast.TFunction (name, _, _, params, _)) ->
      name = func_name && params_are_polymorphic params
      && do_args_instantiate_params arg_types params)
    function_defns
  |> function
  | Some
      (Typed_ast.TFunction (name, maybe_borrowed_ret_ref, return_type, params, body_expr))
    ->
      Some
        (Typed_ast.TFunction
           ( name
           , maybe_borrowed_ret_ref
           , return_type
           , instantiate_params_with_args params arg_types
           , body_expr ))
  | None -> None

let instantiate_all_function_defns function_defns func_insts =
  List.concat_map
    ~f:(fun (func_name, args_types_list) ->
      List.filter_map
        ~f:(fun args_types ->
          instantiate_function_defn_with_args func_name args_types function_defns)
        args_types_list)
    func_insts
  |> fun instantiated_func_defns ->
  (* filter out uninstantiated polymorphic functions *)
  List.filter
    ~f:(fun (Typed_ast.TFunction (_, _, _, params, _)) ->
      not (params_are_polymorphic params))
    function_defns
  |> fun other_func_defns -> List.concat [other_func_defns; instantiated_func_defns]

let desugar_generics_program
    (Typed_ast.Prog (class_defns, function_defns, main_expr) as prog) =
  count_instantiations_program prog
  |> fun (class_insts, func_insts) ->
  instantiate_all_generic_class_defns class_defns class_insts
  |> fun desugared_class_defns ->
  instantiate_all_function_defns function_defns func_insts
  |> fun desugared_func_defns ->
  Typed_ast.Prog (desugared_class_defns, desugared_func_defns, main_expr)
