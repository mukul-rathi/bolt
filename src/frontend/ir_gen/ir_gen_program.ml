open Core
open Ir_gen_expr
open Ir_gen_class_and_function_defns
open Pprint_fir

let ir_gen_program
    (Data_race_checker.Data_race_checker_ast.Prog
      (class_defns, function_defns, main_expr)) =
  let open Result in
  ir_gen_class_defns class_defns
  |> fun ir_class_defns ->
  ir_gen_function_defns class_defns function_defns
  >>= fun ir_function_defns ->
  ir_gen_block_expr class_defns main_expr
  >>| fun ir_main_exprs ->
  Frontend_ir.Prog (ir_class_defns, ir_function_defns, ir_main_exprs)

let pprint_frontend_ir ppf (prog : Frontend_ir.program) = pprint_program ppf prog
