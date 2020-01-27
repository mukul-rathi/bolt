cc_test(
    name = "test_llvm_ir_codegen",
    srcs =  glob(["test_*.cc"]),
    copts = ["-Iexternal/gtest/include"],
    data = glob(["*.compiled"]),
    deps = [
        "@gtest//:main",
        "//src/llvm-backend/llvm_ir_codegen:llvm_ir_codegen",  "@llvm"
    ],
)