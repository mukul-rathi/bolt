# Bolt - Types for Data-Race Freedom

[![CircleCI](https://circleci.com/gh/mukul-rathi/bolt/tree/master.svg?style=svg)](https://circleci.com/gh/mukul-rathi/bolt/tree/master)
[![Coverage Status](https://coveralls.io/repos/github/mukul-rathi/bolt/badge.svg?branch=master)](https://coveralls.io/github/mukul-rathi/bolt?branch=master)

This is the repository for my Part II Dissertation.

I am implementing a programming language Bolt with a type system that eliminates data-races. In OCaml!

## Getting started

The **Makefile** details all the main commands. To get started run these commands!

- `make install` - install dependencies
- `make hook` - install the git pre-commit hook
- `make build` - build the project
- `scripts/run-program.sh <filename> <flag>` - run a Bolt program (extension `.bolt`) - pass in the`-help` flag to see the list of possible flags you can pass in.
- `alias bolt=./scripts/run_program.sh >> ~/.bashrc` - okay this isn't strictly necessary, but running `bolt <filename>` to execute a Bolt program is super cool!

## Project structure

In the `src/frontend` folder

The entrypoint for execution is `compile_program_ir.ml`. This executes the lexing/parsing, type-checking and compiles frontend output to a serialised IR. It can optionally pretty-print the intermediate sASTs.

- `ast/` contains types and pprint utils common to the ASTs
- `parsing/` contains the code for lexing and parsing Bolt programs using OCamllex and Menhir. `lex_and_parse.mli` serves as the main interface for this directory. The type of the output is in `parsed_ast.mli`
- `typing/` contains the type-checking code for the core language. `type_program.mli` serves as the interface to `compile_program_ir`. The typed AST output is in `typed_ast.mli`.
- `desugaring/` contains the desugaring code. `desugar_program.mli` serves as the interface to `compile_program_ir`. The desugared AST output is in `desugared_ast.mli`.
  `desugaring/` contains the serialisable IR generating code. `ir_gen_program.mli` serves as the interface to `compile_program_ir`. The serialisable AST output is in `llvm_ast.mli`. The protoc file is automatically generated from the `llvm_ast.ml` type definitions

## Build

If it seems like there's a lot of files, yes, there are as I've tried to structure this as modularly as possible - utilising the super composability of **dune**.

### Linting and formatting

All OCaml code is formatted using OCamlformat, and linted using Jane Street's open-source OCaml linter.

## Docs

To see the documentation for the modules in the repo, go to [https://bolt.mukulrathi.com/](https://bolt.mukulrathi.com/).

This is automatically built and deployed using Circle CI.

You can get docs locally in the `docs` folder by running `make doc`

## Testing

`make test` runs the entire test suite.

### Unit testing

The unit test suite uses Alcotest (with Qcheck). These can be found under `tests/frontend/alcotest` and are prefixed with `test_`.

### Expect tests

The expect tests use Jane Street's PPX_Expect library and can be found under `tests/frontend/expect`.

### E2E tests

The E2E tests consist of a custom bash script `tests/run_e2e_tests.sh` that executes the main function from the command line on `.bolt` files, and compares the output when each flag is set.

### Test Coverage

Coverage of the master branch is viewable on Coveralls.io - click the badge above! To run the coverage locally run `make coverage` - this will generate the html in the `_coverage` directory.

## Continuous Integration

CircleCI builds the repo and lints it and checks it for formatting. It then runs the test suite, generates test coverage and sends the coverage report to Coveralls.io. Finally, it generates the documentation and deploys it to my website.
