# miniDusa

## Introduction

- say some words about dusa / finite choice logic programming
- motivate minidusa from this:
  - lack of extensibility (macros, builtins, integration)

- to solve, we implemented miniDusa as a hosted DSL in Racket w/ syntax spec
- say some words about what syntax spec is
- we get things in impl for free like syntax highlighting and scoping

[ insert diagram about the pipeline ]
- extensions -expand-> core -checks, expand-> AST -solver-> solutions
- reference Michael's syntax spec stuff

- macros in action
  - convenient way to implement some Dusa features
  - more importantly, abstracts over encoding + convenient notation
  - talk about hygiene (depends on how much is up)
    - avoid too many details for things subject to change
    - by being a dsl hosted in racket, we inherit its macro system and its
      features like hygiene and syntax/parse stuff

[ insert code example, probably graph coloring lol ]
- room to talk about builtins here?? not sure
- maybe dusa code example when talking about that? idk organization

- integration with racket
  - importing arbitrary racket functions as functional relations

- future work (it is a workshop)
  - exploring more expressive racket integrations
  - potentially safer integration using contracts? idk
