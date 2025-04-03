SYNTAX RELATED:
- [ ] Experiment with removing nesting
- [x] Static check for arity
  - [ ] Also throw in reserved keyword check
  - [x] This may depend on if we have syntax in our variables
        instead of just symbols. What is the right equality?
- [ ] Improve `<logic-term>` to support more
  - [ ] Ex: lists
- [ ] Singletons in `choice` should not need `choice`
- [ ] `demand` and `forbid` macros
  - [ ] Depends on freshness of relation symbols
- [ ] Nested symbols -> flatten
- [ ] Better custom error messages

RUNTIME RELATED:
- [x] Implement `choose`
  - [x] Decide whether singleton choice rules are choose or deduce
        (which may involve tweaking the current solver)
- [x] **Builtin support**
  - [x] This also requires making changes to the syntax too
- [ ] How much work to get this working with open rules?
- [ ] Port over Dusa's test suite (once we support more things...)

CODE IMPROVEMENTS:
- [x] Better interfaces for `solution` and `database`?
  - [x] Solutions should maybe be sets, with the right equality
  - [x] Database abstraction to enable replacing with tries?
- [ ] `NONE` getting exposed to end user in solutions
  - [ ] At the very least we can make it so it doesn't get printed
- [ ] Maybe having an `attribute` struct would be better
- [ ] Testing infrastructure improvements
- [ ] Lots of duplication when compiling "attributes" (4x dupe)

QUESTIONS FOR US:
- [ ] Is it useful to know what is a binding vs a reference in our AST?
- [ ] Reconsider `fact` and `make-fact` stuff?

QUESTIONS FOR OH:
- [ ] (Re)exporting some but not all scopes
  - [ ] Specifically, we want `rel-var` scopes across rules, but not `logic-var`s
  - [ ] Our only idea was to inline `attribute`, which we think will work
  - [ ] This _needs_ to work before any of the symbol table stuff does
- [ ] How do we not thread around the symbol tables in annoying ways
- [ ] `rel-var may not be used as a racket expression` error

- [x] Binding structure for built-ins: how can we check what is a built-in
- [ ] Relation symbol freshness
  - [ ] How would we compare at runtime with syntax?
  - [ ] We don't want to have fresh stuff exposed to user
  - [ ] Should we even do this?
- [ ] `_` logic variables
  - [ ] More generally, is there any way for our compiler to generate
        fresh logic variables?
- [ ] Ask about structuring tests and test redundancy / duplication.
  - [ ] Testing expansion vs runtime vs etc
  - [ ] Also keep in mind we may want to export runtime functions
        and the AST separately from each other?
- [ ] What is `#%`? I think just idiom for internal stuff
- [ ] Is there any more “compilation” that we could do, besides to AST?

QUESTIONS (ONE DAY):
- [ ] Should things be `#:transparent`
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)