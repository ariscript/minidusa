BEFORE WEDNESDAY:
- [x] Figure out binding structure
- [x] Tweak spec to use internal hack
  - [x] Just one non-terminal, since it has better error messages
- [ ] Write simple compiler
  - [ ] Once we have this, try to change back to `racket-expr`
  - [ ] Static check for `bind` in conclusions of rules
  - [ ] Static check for arity: just use normal symbol table
- [ ] Prep for code walk
  - [ ] Test both good compilation and errors

NEXT STEPS:
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`
- [ ] Better custom error messages