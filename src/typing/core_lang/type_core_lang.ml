open Core
open Result

let type_core_lang (Parsing.Parsed_ast.Prog (class_defns, function_defns, expr)) =
  (* Check if class defns well-formed *)
  Type_classes.type_class_defns class_defns function_defns
  >>= fun typed_class_defns ->
  Type_functions.type_function_defns class_defns function_defns
  >>= fun typed_function_defns ->
  (* Type check the expression *)
  Type_expr.type_expr class_defns function_defns expr
  >>| fun typed_expr ->
  Typed_ast.Prog (typed_class_defns, typed_function_defns, typed_expr)
