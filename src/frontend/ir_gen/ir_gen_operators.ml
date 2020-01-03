let ir_gen_un_op = function
  | Ast.Ast_types.UnOpNot -> Llvm_ast.UnOpNot
  | Ast.Ast_types.UnOpNeg -> Llvm_ast.UnOpNeg

let ir_gen_bin_op = function
  | Ast.Ast_types.BinOpPlus -> Llvm_ast.BinOpPlus
  | Ast.Ast_types.BinOpMinus -> Llvm_ast.BinOpMinus
  | Ast.Ast_types.BinOpMult -> Llvm_ast.BinOpMult
  | Ast.Ast_types.BinOpIntDiv -> Llvm_ast.BinOpIntDiv
  | Ast.Ast_types.BinOpRem -> Llvm_ast.BinOpRem
  | Ast.Ast_types.BinOpLessThan -> Llvm_ast.BinOpLessThan
  | Ast.Ast_types.BinOpLessThanEq -> Llvm_ast.BinOpLessThanEq
  | Ast.Ast_types.BinOpGreaterThan -> Llvm_ast.BinOpGreaterThan
  | Ast.Ast_types.BinOpGreaterThanEq -> Llvm_ast.BinOpGreaterThanEq
  | Ast.Ast_types.BinOpAnd -> Llvm_ast.BinOpAnd
  | Ast.Ast_types.BinOpOr -> Llvm_ast.BinOpOr
  | Ast.Ast_types.BinOpEq -> Llvm_ast.BinOpEq
  | Ast.Ast_types.BinOpNotEq -> Llvm_ast.BinOpNotEq
