load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "llvm_ir_codegen",
    srcs =  glob(["*.cc"]),
    hdrs = glob(["*.h"]),
    visibility = ["//src/llvm-backend:__pkg__", "//tests/llvm-backend/llvm_ir_codegen:__pkg__"],
    deps = [  "//src/llvm-backend/deserialise_ir:deserialise_ir","@llvm"]

)
