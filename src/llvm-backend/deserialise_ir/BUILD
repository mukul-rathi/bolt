load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "deserialise_ir",
    srcs =  glob(["*.cc"]),
    hdrs = glob(["*.h"]),
    visibility = ["//src/llvm-backend:__pkg__", "//src/llvm-backend/llvm_ir_codegen:__pkg__", "//tests/llvm-backend/deserialise_ir:__pkg__"],
    deps = ["//src:frontend_ir_cc_proto", "@llvm"]
)
