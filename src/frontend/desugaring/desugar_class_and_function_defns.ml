open Desugar_expr
open Desugar_overloading
open Ast.Ast_types
open Core

let borrowed_param_vars params =
  List.filter_map
    ~f:(fun (TParam (_, param_name, _, maybeBorrowed)) ->
      match maybeBorrowed with Some Borrowed -> Some param_name | None -> None)
    params

let get_param_types params =
  List.map ~f:(fun (TParam (param_type, _, _, _)) -> param_type) params

let desugar_function_defn class_defns function_defns
    (Typing.Typed_ast.TFunction
      (func_name, maybe_borrowed_ref_ret, ret_type, params, body_expr)) =
  desugar_block_expr class_defns function_defns (borrowed_param_vars params) body_expr
  |> fun desugared_body_expr ->
  Desugared_ast.TFunction
    ( name_mangle_if_overloaded_function function_defns func_name (get_param_types params)
    , maybe_borrowed_ref_ret
    , ret_type
    , params
    , desugared_body_expr )

let desugar_method_defn class_defns function_defns method_defns
    (Typing.Typed_ast.TMethod
      (method_name, maybe_borrowed_ref_ret, ret_type, params, capabilities_used, body_expr))
    =
  desugar_block_expr class_defns function_defns (borrowed_param_vars params) body_expr
  |> fun desugared_body_expr ->
  Desugared_ast.TMethod
    ( name_mangle_if_overloaded_method method_defns method_name (get_param_types params)
    , maybe_borrowed_ref_ret
    , ret_type
    , params
    , capabilities_used
    , desugared_body_expr )

let desugar_class_defn class_defns function_defns
    (* Generics have been desugared earlier in this stage so we ignore whether a class is
       generic or not. *)
      (Typing.Typed_ast.TClass (class_name, _, capabilities, fields, method_defns)) =
  List.map ~f:(desugar_method_defn class_defns function_defns method_defns) method_defns
  |> fun desugared_method_defns ->
  Desugared_ast.TClass (class_name, capabilities, fields, desugared_method_defns)
