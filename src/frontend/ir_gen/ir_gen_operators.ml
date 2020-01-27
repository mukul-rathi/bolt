let ir_gen_un_op = function
  | Ast.Ast_types.UnOpNot -> Frontend_ir.UnOpNot
  | Ast.Ast_types.UnOpNeg -> Frontend_ir.UnOpNeg

let ir_gen_bin_op = function
  | Ast.Ast_types.BinOpPlus -> Frontend_ir.BinOpPlus
  | Ast.Ast_types.BinOpMinus -> Frontend_ir.BinOpMinus
  | Ast.Ast_types.BinOpMult -> Frontend_ir.BinOpMult
  | Ast.Ast_types.BinOpIntDiv -> Frontend_ir.BinOpIntDiv
  | Ast.Ast_types.BinOpRem -> Frontend_ir.BinOpRem
  | Ast.Ast_types.BinOpLessThan -> Frontend_ir.BinOpLessThan
  | Ast.Ast_types.BinOpLessThanEq -> Frontend_ir.BinOpLessThanEq
  | Ast.Ast_types.BinOpGreaterThan -> Frontend_ir.BinOpGreaterThan
  | Ast.Ast_types.BinOpGreaterThanEq -> Frontend_ir.BinOpGreaterThanEq
  | Ast.Ast_types.BinOpAnd -> Frontend_ir.BinOpAnd
  | Ast.Ast_types.BinOpOr -> Frontend_ir.BinOpOr
  | Ast.Ast_types.BinOpEq -> Frontend_ir.BinOpEq
  | Ast.Ast_types.BinOpNotEq -> Frontend_ir.BinOpNotEq
