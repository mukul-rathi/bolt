# Bolt - Data-Race Freedom Baked In

[![CircleCI](https://circleci.com/gh/mukul-rathi/bolt/tree/master.svg?style=svg)](https://circleci.com/gh/mukul-rathi/bolt/tree/master)
[![Coverage Status](https://coveralls.io/repos/github/mukul-rathi/bolt/badge.svg?branch=master)](https://coveralls.io/github/mukul-rathi/bolt?branch=master)

## What, another programming language?

Yep, this one prevents data races! Concurrent code is hard to get right, so let the language take care of it for you! The best part is that you get **more fine-grained concurrency** than Rust and this language doesn't get in the way when you want to write single-threaded code. Want to write Rusty ownership-style code - yep, you can do that in Bolt too!

For a description of the theory, check out the [accompanying dissertation](https://github.com/mukul-rathi/bolt-dissertation).

## Alright so what does this language do?

You can already write a lot of Java-esque code - see `examples/` in this repo. Bolt already supports inheritance, method overloading and overriding, and generics. Is Bolt missing something? Comment on [this issue](https://github.com/mukul-rathi/bolt/issues/134) - I'm all ears.

Two ways Bolt differs from traditional languages - the capability annotations for fields and function/method type signatures, and a _structured_ approach to concurrency - so you know exactly how long your threads live for:

```
finish{
  async{ // fork a thread using the async command - you can spawn as many as you like!
    ... // execute an expression in the forked thread
  }
  ... // continue executing your code on current thread

} // all forked threads finish executing here
```

## Wait, how did you build this?

A lot of trial-and-error and experimenting with reverse-engineering C++ code! I'll save you the trouble - step-by-step tutorials for how I built this all are incoming - I'll tweet about them when they drop so [follow me on Twitter](https://twitter.com/mukulrathi_). **Update: [all the posts are live](https://mukulrathi.com/create-your-own-programming-language/intro-to-compiler/)**.

Unlike your run-of-the-mill compiler tutorials, we'll be talking about more advanced language features too, like generics, inheritance, method overloading and overriding!

## Getting started

Bolt's compiler is written in OCaml and C++. You'll need OCaml and `opam` installed. You'll also need to install Bazel.

Note you don't need to have LLVM installed - just update `llvm.bzl` to use the [right pre-built LLVM Binary](https://releases.llvm.org/download.html), and Bazel will download and extract that binary for you!

Once you have these installed, the **Makefile** details all the main commands.
To get started run these commands!

- `make install` - install dependencies
- `make hook` - install the git pre-commit hook
- `make build` - build the project

To compile a program:

- `scripts/compile_program.sh <filename> <flag>` - run a Bolt program (extension `.bolt`) - pass in the`-help` flag to see the list of possible flags you can pass in.
- `alias boltc=./scripts/compile_program.sh >> ~/.bashrc`

To compile **and run** the program:

- `scripts/run_program.sh <filename> <flag>` - run a Bolt program (extension `.bolt`) - pass in the`-help` flag to see the list of possible flags you can pass in.
- `alias bolt=./scripts/run_program.sh >> ~/.bashrc` - okay this isn't strictly necessary, but running `bolt <filename>` to execute a Bolt program is super cool!

Okay, - the `boltc` and `bolt` aliasing isn't strictly necessary, but running `boltc <filename>` to compile and `bolt <filename>` to run a Bolt program is super cool!

Check out the `REPO_OVERVIEW.md` file for more details about the project structure.
