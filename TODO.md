SYNTAX RELATED:
- [x] Static check for arity
  - [x] Also throw in reserved keyword check
  - [x] This may depend on if we have syntax in our variables
        instead of just symbols. What is the right equality?
- [x] Change over to using `id` for relation variables
- [ ] `#demand` and `#forbid` with `gensym`
  - Maybe we should slightly expand the syntax-spec
  - Layer on top which expands to that other syntax-spec stuff
  - This is where we implement the name hiding stuff
- [ ] Concrete syntax
  - [ ] Infix `is`?
  - [ ] Maybe make some singletons not need parens? idk
- [ ] Improve `<logic-term>` to support more
  - [ ] Ex: lists
- [ ] Nested symbols -> expand with extra premises

COMPILER RELATED:
- [x] Change compilers to use normal tables
  - [x] Also, we can use `parameter`s to avoid threading

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
  - [ ] Make sure that we don't have generated symbols either
- [ ] Maybe having an `attribute` struct would be better
  - [ ] Currently, lots of duplication when compiling attributes
- [ ] Testing infrastructure improvements

QUESTIONS FOR US:
- [ ] Is it useful to know what is a binding vs a reference in our AST?
- [ ] Reconsider `fact` and `make-fact` stuff?

QUESTIONS FOR OH:
- [x] How do we not thread around the symbol tables in annoying ways
- [ ] Possible to get the demo code?
- [x] Binding structure for built-ins: how can we check what is a built-in
- [ ] `_` logic variables
  - [ ] More generally, is there any way for our compiler to generate
        fresh logic variables?
- [ ] Ask about structuring tests and test redundancy / duplication.
  - [ ] Testing expansion vs runtime vs etc
  - [ ] Also keep in mind we may want to export runtime functions
        and the AST separately from each other?
- [ ] Is there any more “compilation” that we could do, besides to AST?

QUESTIONS (ONE DAY):
- [ ] Should things be `#:transparent`
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)