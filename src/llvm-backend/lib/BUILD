load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "hello-world",
    srcs =  glob(["*.cc"]),
    hdrs = glob(["*.h"]),
    visibility = ["//src/llvm-backend/main:__pkg__", "//tests/llvm-backend:__pkg__"],
)
