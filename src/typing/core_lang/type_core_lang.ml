open Core
open Result

let type_core_lang (Parsed_ast.Prog (class_defns, trait_defns, expr)) =
  (* Check if trait defns well-formed *)
  Type_traits.type_trait_defns trait_defns
  >>= fun () ->
  (* Check if class defns well-formed *)
  Type_classes.type_class_defns class_defns trait_defns
  >>= fun () ->
  (* Type check the expression *)
  Type_expr.type_expr class_defns trait_defns expr
  >>| fun typed_expr -> Typed_ast.Prog (class_defns, trait_defns, typed_expr)
