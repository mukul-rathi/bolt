open Core
open Type_generics
open Parsing

let type_program (Parsed_ast.Prog (class_defns, function_defns, main_expr)) =
  let open Result in
  (* Check if class defns well-formed *)
  Type_classes.type_class_defns class_defns function_defns
  >>= fun typed_class_defns ->
  Type_functions.type_function_defns class_defns function_defns
  >>= fun typed_function_defns ->
  type_generics_usage_block_expr main_expr None
  >>= fun () ->
  (* Type check the expression *)
  Type_expr.type_block_expr class_defns function_defns main_expr []
  >>| fun (typed_main_expr, _) ->
  Typed_ast.Prog (typed_class_defns, typed_function_defns, typed_main_expr)

let pprint_typed_ast ppf (prog : Typed_ast.program) = Pprint_tast.pprint_program ppf prog
