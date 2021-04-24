# Refinement Type Checker

This repository contains a (currently WIP) small language
meant to showcase refinement type checking and refinement
type inference.

The language itself consists of the following constructs:

- Integer and boolean literals: `1`, `-4`, `false`
- Let bindings: `let _ = _ in _`
- Lambdas: `fn _ -> _`
- If-expressions: `if _ then _ else _`
- Arithmetic and logical operators: `+`, `-`, `*`, `/`, `%`, `<`, `==`, `and`, `or`, `not`

The type system has `int`, `bool`, and function types.

# The algorithm

First, source of the language is parsed from the syntax
given in `syntax.mly`. Then, the normal Hindley-Milner
type checker `hindley_milner.ml` runs on the `Ast` outputting
a `TypedAst`.

After this, the `refinement.ml` pass is run which collects
constraints and feeds them to Z3 to solve.
