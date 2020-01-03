open Core
open Ir_gen_expr

let ir_gen_type = function
  | Ast.Ast_types.TEBool -> Llvm_ast.TEBool
  | Ast.Ast_types.TEInt -> Llvm_ast.TEInt
  | Ast.Ast_types.TEVoid -> Llvm_ast.TEVoid
  | Ast.Ast_types.TEClass class_name ->
      Llvm_ast.TEClass (Ast.Ast_types.Class_name.to_string class_name)

let ir_gen_param = function
  | Ast.Ast_types.TParam (param_type, param_name, _) ->
      Llvm_ast.TParam (ir_gen_type param_type, Ast.Ast_types.Var_name.to_string param_name)
  | Ast.Ast_types.TVoid -> Llvm_ast.TVoid

let ir_gen_field_defn (Ast.Ast_types.TField (_, field_type, field_name, _)) =
  Llvm_ast.TField (ir_gen_type field_type, Ast.Ast_types.Field_name.to_string field_name)

let ir_gen_class_defn (Desugaring.Desugared_ast.TClass (class_name, _, fields, _)) =
  List.map ~f:ir_gen_field_defn fields
  |> fun ir_fields ->
  Llvm_ast.TClass (Ast.Ast_types.Class_name.to_string class_name, ir_fields)

let ir_gen_class_defns class_defns = List.map ~f:ir_gen_class_defn class_defns

let ir_gen_class_method_defn class_name
    (Desugaring.Desugared_ast.TMethod (method_name, return_type, params, _, body_expr)) =
  let open Result in
  let obj_type = Ast.Ast_types.TEClass class_name in
  ir_gen_method_name method_name obj_type
  >>= fun ir_method_name ->
  ir_gen_type return_type
  |> fun ir_return_type ->
  Llvm_ast.TParam (ir_gen_type obj_type, "this") :: List.map ~f:ir_gen_param params
  |> fun ir_params ->
  Result.all (List.map ~f:ir_gen_expr body_expr)
  >>| fun ir_body_expr ->
  Llvm_ast.TFunction (ir_method_name, ir_return_type, ir_params, ir_body_expr)

let ir_gen_class_method_defns
    (Desugaring.Desugared_ast.TClass (class_name, _, _, method_defns)) =
  Result.all (List.map ~f:(ir_gen_class_method_defn class_name) method_defns)

let ir_gen_function_defn
    (Desugaring.Desugared_ast.TFunction (func_name, return_type, params, body_expr)) =
  let open Result in
  ir_gen_type return_type
  |> fun ir_return_type ->
  List.map ~f:ir_gen_param params
  |> fun ir_params ->
  Result.all (List.map ~f:ir_gen_expr body_expr)
  >>| fun ir_body_expr ->
  Llvm_ast.TFunction
    ( Ast.Ast_types.Function_name.to_string func_name
    , ir_return_type
    , ir_params
    , ir_body_expr )

let ir_gen_function_defns class_defns function_defns =
  let open Result in
  Result.all (List.map ~f:ir_gen_class_method_defns class_defns)
  >>= fun ir_classes_method_defns ->
  Result.all (List.map ~f:ir_gen_function_defn function_defns)
  >>| fun ir_function_defns -> List.concat (ir_function_defns :: ir_classes_method_defns)
