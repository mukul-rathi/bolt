"""Build definitions for LLVM

Credit goes to GitHub user dgp1130 - original is from https://github.com/dgp1130/llvm-bazel-foolang/blob/master/llvm.bzl

"""

def _impl(ctx):
    """Implementation of the llvm() rule

    Downloads and extracts the pre-built LLVM binaries and compiles them into a cc_library().
    """
    # Download the pre-built LLVM binaries.
    version = "9.0.0"
    name = "clang+llvm-%s-x86_64-darwin-apple" % version
    ctx.download_and_extract(
        url = "https://releases.llvm.org/%s/%s.tar.xz" % (version, name),
    )

    # Create a bash wrapper of llc to invoke it from Bazel.
    ctx.file("llc_runner.sh", """
# --- begin runfiles.bash initialization ---
if [[ ! -d "${RUNFILES_DIR:-/dev/null}" && ! -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
    if [[ -f "$0.runfiles_manifest" ]]; then
      export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
    elif [[ -f "$0.runfiles/MANIFEST" ]]; then
      export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
    elif [[ -f "$0.runfiles/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
      export RUNFILES_DIR="$0.runfiles"
    fi
fi
if [[ -f "${RUNFILES_DIR:-/dev/null}/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
  source "${RUNFILES_DIR}/bazel_tools/tools/bash/runfiles/runfiles.bash"
elif [[ -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  source "$(grep -m1 "^bazel_tools/tools/bash/runfiles/runfiles.bash " \
            "$RUNFILES_MANIFEST_FILE" | cut -d ' ' -f 2-)"
else
  echo >&2 "ERROR: cannot find @bazel_tools//tools/bash/runfiles:runfiles.bash"
  exit 1
fi
# --- end runfiles.bash initialization ---

# Pass through invocation to the underlying llc binary
$(rlocation %s/external/llvm/%s/bin/llc) $@
""" % (ctx.attr.workspace_name, name))

    # Create a BUILD file exporting the LLVM libraries and an llc binary.
    ctx.file("BUILD", """
package(default_visibility = ["//visibility:public"])

# Include all LLVM code into the single target
cc_library(
    name = "llvm",
    # Don't include lib/ recursively, because we don't want all the clang code in there.
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

# Wrap llc in a Bazel invocable binary.
sh_binary(
    name = "llc",
    srcs = ["llc_runner.sh"],
    data = ["%s/bin/llc"],
    deps = ["@bazel_tools//tools/bash/runfiles"],
)
""" % (name, name, name, name, name, name, name))

    # Create a file for Skylark macros for LLVM.
    ctx.file("llvm.bzl", '''
def llc(name, src, out):
    """Invokes llc on the given src file and outputs it at the given out name."""
    
    native.genrule(
        name = name,
        srcs = [src],
        outs = [out],
        cmd = """
            $(location @llvm//:llc) < "$<" > "$@"
        """,
        tools = ["@llvm//:llc"],
    )
''')

    # Create an empty WORKSPACE file.
    ctx.file("WORKSPACE", "")

# Declare the rule to import LLVM from a WORKSPACE file.
llvm = repository_rule(
    implementation = _impl,
    attrs = {
        # The version of LLVM to download.
        "version": attr.string(default = "3.9.1"),
        # The name of the workspace() this is being included in.
        "workspace_name": attr.string(mandatory = True),
    },
)