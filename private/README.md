We split our implementation into a few different files:

- `spec.rkt`: the main `syntax-spec`, and `logic` macro
  - contains tests for parsing and compiling
- `compile.rkt`: the compiler from DSL syntax to our AST
  (as Racket syntax)
- `runtime.rkt`: the runtime system: AST and the solver
  - some tests for solving simple programs from AST
- `database.rkt`: abstraction over the auxiliary data structure
  so we can change our internal representation for efficiency
- `data.rkt`: definitions for common structures

The `runtime.rkt` file contains relatively comprehensive
developer-facing documentation as comments throughout.
