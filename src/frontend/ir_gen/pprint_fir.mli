(** This module pretty prints the serialisable frontend IR of a Bolt program *)

val pprint_program : Format.formatter -> Frontend_ir.program -> unit
