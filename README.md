# Haskell Lexer PoC
An interesting usecase for functional programming.

This project uses Cabal for building and running the executables.
Install Cabal here: https://www.haskell.org/cabal/ \
You can build + run one of the following executables.

## Full Tokenizer
Runs a tokenizer on your input that recognizes some common Java tokens like identifiers, keywords, strings and comments.
Then outputs whether the whole input has been tokenized successfully and lists the successful tokens.

Usage:
Go to the project root, then execute the following commands with cabal installed:
- `cabal build tokenizer`
- Run on direct input: `cabal run tokenizer -- <input>`
- Run on file: `cabal run tokenizer -- -f <filepath>`
- Run and output to file: `cabal run tokenizer -- (args) > (output-filepath)`

## Simple Automaton
Executes a single automaton on the given input.
The automaton only accepts the Java keywords "public", "static", "class" and "void".

Usage:
Go to the project root, then execute the following commands with cabal installed:
- `cabal build simple`
- `cabal run simple -- <input>`
