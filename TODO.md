BEFORE WEDNESDAY:
- [x] Figure out binding structure
- [ ] Write simple compiler
  - [ ] Once we have this, try to change back to `racket-expr`
  - [ ] Static check for `bind` in conclusions of rules
  - [ ] Static check for arity: just use normal symbol table
- [ ] Tweak spec to use internal hack
  - [ ] Just one non-terminal, since it has better error messages
- [ ] Prep for code walk
  - [ ] Test both good compilation and errors

NEXT STEPS:
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`
- [ ] Better custom error messages

TODO:
- Make a minimal test case for `string`