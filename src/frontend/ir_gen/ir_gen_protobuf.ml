open Frontend_ir

let ir_gen_protobuf program out_chan =
  let protobuf_message = Protobuf.Encoder.encode_exn program_to_protobuf program in
  output_bytes out_chan protobuf_message
