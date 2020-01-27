(** Generates the serialisable IR representation of the operators *)

val ir_gen_un_op : Ast.Ast_types.un_op -> Frontend_ir.un_op
val ir_gen_bin_op : Ast.Ast_types.bin_op -> Frontend_ir.bin_op
