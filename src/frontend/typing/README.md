## Typing

The stage type-checks the core language (this is the "traditional" form of type-checking found in most compilers - the data-race type-checker is a separate stage).

This stage occurs after the lexing+parsing - `type_program.mli` acts as the interface to the library - this takes in the program's parsed AST and outputs the Typed AST. It also has a pretty-print function to print out the typed AST.

The `typed_ast.mli` contains the type definition for the typed AST.

The rest of this folder type-checks specific language constructs. The main file is `type_expr.mli` which type-checks a given Bolt expression. This is used by the `type_classes.mli` and `type_functions.mli` to check function and method bodies.

Function/method overloading, inheritance and generics are language constructs are type-checked in their own files.

Finally `type_env.ml` contains a bunch of helper functions used in the stage.
