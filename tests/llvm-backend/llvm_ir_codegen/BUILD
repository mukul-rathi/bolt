load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
name = "ir_codegen_test_visitor",
 srcs =  ["ir_codegen_test_visitor.cc"],
    hdrs =  ["ir_codegen_test_visitor.h"],
    deps = [
        "//src/llvm-backend/llvm_ir_codegen:llvm_ir_codegen",  "@llvm"
    ],
)

cc_test(
    name = "test_llvm_ir_codegen",
    srcs =  glob(["test_*.cc"]),
    copts = ["-Iexternal/gtest/include"],
    deps = [
        "@gtest//:main",
        "//src/llvm-backend/llvm_ir_codegen:llvm_ir_codegen", "//tests/llvm-backend/llvm_ir_codegen:ir_codegen_test_visitor",  "@llvm"
    ],
)