BEFORE WEDNESDAY:
- [x] Figure out binding structure
- [x] Tweak spec to use internal hack
  - [x] Just one non-terminal, since it has better error messages
- [x] Write simple compiler
  - [x] Static check for `bind` in conclusions of rules
  - [ ] Static check for arity: just use normal symbol table
    - [ ] Also throw in reserved keyword check
  - [ ] Once we have this, try to change back to `racket-expr`?
- [ ] Prep for code walk
  - [ ] Test both good compilation and errors

NEXT STEPS:
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`
  - [ ] Decide whether singleton choice rules are choose or deduce
        (which may involve tweaking the current solver)
- [ ] Better custom error messages
- [ ] Make things `#:transparent` maybe?
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)