val ir_gen_protobuf : Frontend_ir.program -> out_channel -> unit
(** Given the IR from a program, write the serialised protobuf bytes output to the
    [out_channel] *)
