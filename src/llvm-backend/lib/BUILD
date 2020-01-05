load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "hello-world",
    srcs =  glob(["*.cc"]),
    hdrs = glob(["*.h"]),
    visibility = ["//src/llvm-backend/main:__pkg__", "//tests/llvm-backend:__pkg__"],
    deps = [":frontend_ir_cc_proto"]
)

# Convention:
# A cc_proto_library that wraps a proto_library named foo_proto
# should be called foo_cc_proto.
cc_proto_library(
    name = "frontend_ir_cc_proto",
    deps = [":frontend_ir_proto"],
)

# Conventions:
# 1. One proto_library rule per .proto file.
# 2. A file named foo.proto will be in a rule named foo_proto.
proto_library(
    name = "frontend_ir_proto",
    srcs = ["frontend_ir.proto"],
)