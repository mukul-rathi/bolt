"""
Exposes the LLVM binary as a cc_library
"""

def _impl(ctx):
    # Download the pre-built LLVM binary.
    version = "9.0.0"
    name = "clang+llvm-%s-x86_64-darwin-apple" % version
    ctx.download_and_extract(
        url = "https://releases.llvm.org/%s/%s.tar.xz" % (version, name),
    )

    # Create a BUILD file containing the cc_library declaration
    ctx.file("BUILD", """
package(default_visibility = ["//visibility:public"])

cc_library(
    name = "llvm",
    srcs = glob(["%s/lib/*.a"]),
    hdrs = glob([
        "%s/include/**/*.h",
        "%s/include/**/*.inc",
        "%s/include/**/*.def",
        "%s/include/**/*.gen",
    ]),
    includes = ["%s/include/"],
    linkopts = [
        "-lpthread",
        "-lcurses",
        "-ldl",
    ],
)
""" % (name, name, name, name, name, name))

    # Create an empty WORKSPACE file.
    ctx.file("WORKSPACE", "")

llvm = repository_rule(
    implementation = _impl,
    attrs = {
        "workspace_name": attr.string(mandatory = True),
    },
)
