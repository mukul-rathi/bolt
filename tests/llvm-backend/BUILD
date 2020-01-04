cc_test(
    name = "test_llvm_backend",
    srcs =  glob(["test_*.cc"]),
    copts = ["-Iexternal/gtest/include"],
    deps = [
        "@gtest//:main",
        "//src/llvm-backend/lib:hello-world",
    ],
)