# Project structure

In case you already want to have a look round! Don't worry I'll have tutorials up in due course :)

Documentation for the frontend is [here](https://mukul-rathi.github.io/bolt/).

The overall pipeline is as follows:

parsing -> typing -> desugaring -> data_race_checker -> ir_gen (frontend)

-> deserialise_ir -> llvm_ir_codegen (llvm-backend)

## Frontend

In the `src/frontend` folder:

The entrypoint for execution is `compile_program_ir.ml`. This executes the lexing/parsing, type-checking and compiles frontend output to a serialised IR. It can optionally pretty-print the intermediate ASTs.

- `ast/` contains types and pprint utils common to the ASTs

The following folders correspond to each of the frontend pipeline stages (listed here in pipeline order):

- `parsing/` contains the code for lexing and parsing Bolt programs using OCamllex and Menhir. `lex_and_parse.mli` serves as the main interface for this directory. The type of the output is in `parsed_ast.mli`
- `typing/` contains the type-checking code for the core language. `type_program.mli` serves as the interface to `compile_program_ir.ml`. The typed AST output is in `typed_ast.mli`.
- `desugaring/` contains the desugaring code - this is used to simplify the AST for data-race type-checking. `desugar_program.mli` serves as the interface to `compile_program_ir.ml`. The desugared AST output is in `desugared_ast.mli`.
- `data_race_checker/` contains the data-race type-checking code - `type_data_races_program.mli` serves as the interface to `compile_program_ir.ml`. The data-race type-checking operates on the desugared AST.
- `ir_gen/` contains the serialisable IR generating code. `ir_gen_program.mli` serves as the interface to `compile_program_ir.ml`. The serialisable IR type definition is in `frontend_ir.mli`. The `src/frontend_ir.proto` file is automatically generated from the `frontend_ir.ml` type definitions

## LLVM Backend

In the `src/llvm_backend` folder:

- `deserialise_ir/` - this is responsible for deserialising the Protobuf frontend IR output of the frontend and contains equivalent C++ type definitions for the IR.
- `llvm_ir_codegen/` - this contains the code to generate the LLVM IR from the frontend IR.

The `main.cc` is the entrypoint for the LLVM backend.

# Build

The OCaml frontend uses the **Dune** build system, and the C++ LLVM backend uses **Bazel**.

## Linting and formatting

All OCaml code is formatted using OCamlformat, and linted using Jane Street's open-source OCaml linter.

We use clang-format for the C++ code.

# Docs

To see the documentation for the frontend's OCaml modules in the repo, go to [https://mukul-rathi.github.io/bolt/](https://mukul-rathi.github.io/bolt/).

This is automatically built and deployed using Circle CI.

You can get docs locally in the `docs` folder by running `make doc`

# Testing

`make test` runs the entire test suite.

## Unit testing

The unit test suite uses Alcotest (with Qcheck). These can be found under `tests/frontend/alcotest` and are prefixed with `test_`.

## Expect tests

The expect tests use Jane Street's PPX_Expect library and can be found under `tests/frontend/expect`.

## Frontend Integration tests

The frontend IR tests consist of a custom bash script `tests/run_frontend_integration_tests.sh` that executes the main function from the command line on `.bolt` files, and compares the AST output when each flag is set.

## E2E tests

The frontend IR tests consist of a custom bash script `tests/run_e2e_tests.sh` that executes the main function from the command line on `.bolt` files, and compares the LLVM IR output and the program execution output.

## Test Coverage

Coverage of the master branch is viewable on Coveralls.io - click the badge above! To run the coverage locally run `make coverage` - this will generate the html in the `_coverage` directory.

# Continuous Integration

CircleCI builds the repo and lints it and checks it for formatting. It then runs the test suite, generates test coverage and sends the coverage report to Coveralls.io. Finally, it generates the documentation and deploys it to [this site](https://mukul-rathi.github.io/bolt/).
