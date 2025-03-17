BEFORE WEDNESDAY:
- [x] Figure out binding structure
- [ ] Write simple compiler
- [ ] Tweak spec to use internal hack
- [ ] Prep for code walk

NEXT STEPS:
- [ ] Static check for symbol arity (and choose)
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`


QUESTIONS:
- How does the `lookup` hack actually work?
- When should we discern between similar non-terminals with slightly
  different static information? Tradeoff: duplication vs expressivity
- Is there `racket-datum` or similar? What about `symbol`?
  - Any advice on having `expr` but not having everything get parsed as that?
    Maybe this will be easier once we actually compile to things
- How much should our compile-time functions look like `define-syntax`?
  Specifically, I'm thinking about having extra syntax in front of things
  which we are compiling, which would be needed for macros but not for
  compile-time functions on syntax