BEFORE WEDNESDAY:
- [x] Figure out binding structure
- [ ] Write simple compiler
  - [ ] Once we have this, try to change back to `racket-expr`
- [ ] Tweak spec to use internal hack
- [ ] Prep for code walk
  - [ ] Test both good compilation and errors

NEXT STEPS:
- [ ] Static check for symbol arity (and choose)
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`
- [ ] Better custom error messages


QUESTIONS:
- How does the `lookup` hack actually work?
- When should we discern between similar non-terminals with slightly
  different static information? Tradeoff: duplication vs expressivity
- Is there `racket-datum` or similar? What about `symbol`?
  - Any advice on having `expr` but not having everything get parsed as that?
    Maybe this will be easier once we actually compile to things
- Ask about having a macro vs having a compile-time helper on syntax,
  and what about recursive helpers? Refer to PEG and router examples
  - Was the Router example because things wouldn't be expanded otherwise?
  - What about mixing them? We want _some_ functions, so why not all functions?
- Is there documentation on how to do the arity checking,
  similarly to how you do it in mini-Kanren?
- Do we need to match against the host-expression stuff? Surely not
- Why would we have `define-foo` vs `make-foo` and have users define?