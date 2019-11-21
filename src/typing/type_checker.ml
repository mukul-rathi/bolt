open Core
open Result

let type_check_program ~check_data_races
    (Parsed_ast.Prog (class_defns, trait_defns, expr)) =
  (* Check if trait defns well-formed *)
  Type_traits.type_trait_defns trait_defns
  >>= fun () ->
  (* Check if class defns well-formed *)
  Type_classes.type_class_defns class_defns trait_defns
  >>= fun () ->
  (* Type check the expression *)
  Type_expr.type_expr trait_defns class_defns expr ~check_data_races
  >>| fun typed_expr -> Typed_ast.Prog (class_defns, trait_defns, typed_expr)

let pprint_typed_ast ppf (prog : Typed_ast.program) = Pprint_tast.pprint_program ppf prog
