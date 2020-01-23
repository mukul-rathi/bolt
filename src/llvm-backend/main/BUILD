load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    deps = [
        "//src/llvm-backend/deserialise_ir:deserialise_ir",  "//src/llvm-backend/llvm_ir_codegen:llvm_ir_codegen", "@llvm"
    ],
)
