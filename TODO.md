SYNTAX RELATED:
- [ ] Experiment with removing nesting
- [ ] Static check for arity
  - [ ] Also throw in reserved keyword check
  - [ ] This may depend on if we have syntax in our variables
        instead of just symbols. What is the right equality?
- [ ] Improve `<logic-term>` to support more
  - [ ] Ex: lists
- [ ] `_` logic variables
- [ ] Singletons in `choice` should not need `choice`
- [ ] `demand` and `forbid` macros
  - [ ] Depends on freshness of relation symbols
- [ ] Nested symbols -> flatten
- [ ] Better custom error messages

RUNTIME RELATED:
- [x] Implement `choose`
  - [x] Decide whether singleton choice rules are choose or deduce
        (which may involve tweaking the current solver)
- [ ] **Builtin support**
  - [ ] This also requires making changes to the syntax too
- [ ] How much work to get this working with open rules?
- [ ] Port over Dusa's test suite (once we support more things...)

CODE IMPROVEMENTS:
- [ ] Better interfaces for `solution` and `database`?
  - [ ] Solutions should maybe be sets, with the right equality
  - [ ] Database abstraction to enable replacing with tries?
- [ ] Maybe having an `attribute` struct would be better
- [ ] Testing infrastructure improvements

QUESTIONS FOR US:
- [ ] Is it useful to know what is a binding vs a reference in our AST?
- [ ] Reconsider `fact` and `make-fact` stuff?

QUESTIONS FOR OH:
- [ ] Ask about structuring tests and test redundancy / duplication.
  - [ ] Testing expansion vs runtime vs etc
  - [ ] Also keep in mind we may want to export runtime functions
        and the AST separately from each other?
- [ ] What is `#%`?
- [ ] Is there any more “compilation” that we could do, besides to AST?
- [ ] If we don’t want to destroy hygiene, how should we go about that?

QUESTIONS (ONE DAY):
- [ ] Should things be `#:transparent`
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)