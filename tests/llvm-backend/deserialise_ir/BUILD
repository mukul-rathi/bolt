cc_test(
    name = "test_deserialise_ir",
    srcs =  glob(["test_*.cc"]),
    copts = ["-Iexternal/gtest/include"],
    data = glob(["*.compiled"]),
    deps = [
        "@gtest//:main",
        "//src/llvm-backend/deserialise_ir:deserialise_ir",
    ],
)