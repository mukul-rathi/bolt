open Ast.Ast_types
open Core
open Result
open Typing_core_lang

(********** GETTER METHODS for type-checking data races *********)

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

let get_typed_class_defn class_name class_defns loc =
  let matching_class_defns =
    List.filter
      ~f:(fun (Typed_ast.TClass (name, _, _, _)) -> class_name = name)
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

let get_type_capability type_expr class_defns loc =
  match type_expr with
  | TEClass class_name ->
      get_typed_class_defn class_name class_defns loc
      >>| fun (Typed_ast.TClass (_, TCapTrait (cap, _), _, _)) -> cap
  | TECapTrait (TCapTrait (cap, _)) -> Ok cap
  | _ ->
      Error
        (Error.of_string
           (Fmt.str "%s Type doesn't contain capability@." (string_of_loc loc)))

let get_method_body_expr method_name obj_type class_defns loc =
  match obj_type with
  | TEClass class_name ->
      get_typed_class_defn class_name class_defns loc
      >>= fun (Typed_ast.TClass (_, _, _, method_defns)) ->
      get_function_body_expr method_name method_defns loc
  | _                  ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error: %s is not an object type@." (string_of_loc loc)
              (string_of_type obj_type)))
