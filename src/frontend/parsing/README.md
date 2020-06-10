## Parsing

This folder contains the code for the parsing stage of the compiler.

There is an [accompanying tutorial](https://mukulrathi.netlify.app/create-your-own-programming-language/parsing-ocamllex-menhir/).

`lexer.mll` and `parser.mly` are the specifications for the lexer (which uses OCamllex) and parser (which uses Menhir) respectively.

The `parsed_ast.mli` file contains the type definition for the parsed Abstract Syntax Tree.

There are a couple of modules to pretty print tokens and the parsed Abstract Syntax Tree.

Finally, the main interface for this parsing library is `lex_and_parse.mli` - this exposes a function that takes in a buffer that the lexer reads from, and returns the parsed AST. It also exposes a pretty-print function to view the output parsed AST.
