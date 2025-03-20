SYNTAX RELATED:
- [ ] Static check for arity: just use normal symbol table
  - [ ] Also throw in reserved keyword check
- [ ] Improve `<logic-term>` to support more
- [ ] Better custom error messages

RUNTIME RELATED:
- [ ] Implement `choose`
  - [ ] Decide whether singleton choice rules are choose or deduce
        (which may involve tweaking the current solver)
- [ ] Better interfaces for `solution` and `database`


QUESTIONS FOR OH:
- [ ] Ask about structuring tests and test redundancy / duplication.
  - [ ] Testing expansion vs runtime vs etc
- [ ] What is `#%`?
- [ ] Is there any more “compilation” that we could do, besides to AST?
- [ ] If we don’t want to destroy hygiene, how should we go about that?

QUESTIONS (ONE DAY):
- [ ] Should things be `#:transparent`
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)