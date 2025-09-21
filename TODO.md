SYNTAX RELATED:
- [x] Static check for arity
  - [x] Also throw in reserved keyword check
  - [x] This may depend on if we have syntax in our variables
        instead of just symbols. What is the right equality?
- [x] Change over to using `id` for relation variables
- [ ] Concrete syntax
  - [x] Infix `is`?
  - [ ] Maybe make some singletons not need parens? idk
- [x] Improve `<logic-term>` to support more
  - [x] Ex: lists
- [ ] Actually support `_`s
- [ ] Nested symbols -> expand with extra premises
- [ ] Be able to import things in a macro itself?
  - Would probably require changing `decls`

COMPILER RELATED:
- [x] Change compilers to use normal tables
- [ ] Consider using `parameter`s to avoid threading
  - [ ] Maybe pull `raise-if-imported`

RUNTIME RELATED:
- [x] Implement `choose`
  - [x] Decide whether singleton choice rules are choose or deduce
        (which may involve tweaking the current solver)
- [x] **Builtin support**
  - [x] This also requires making changes to the syntax too
- [x] How much work to get this working with open rules?
- [ ] Port over Dusa's test suite (once we support more things...)

SYNTAX OBJECTS:
- [ ] Keep syntax objects around at runtime and compare them for equality using
  `bound-identifier=?`
- [ ] In general, filter fresh relation names out of solutions
  - `syntax-original?` on syntax objects can tell if it has ever come from the
    template of a macro as opposed to being original syntax
  - syntax-spec may not preserve this; we may have to try using the
    `compiled-from` property too which maps back to the surface syntax first
    - We may need to do this anyways to display the underlying symbol (or use
      `syntax->datum`)
- [ ] Extend with test features to be able to test fresh relation names
  - We may want a deterministic name mangler in these cases (try not to be too
    fragile though!), or maybe some "up to alpha equivalence" helpers
  - We might be able to get away with just observing these effects on public
    relations, but these could still be useful even for print debugging
  - This should definitely go in a different module

- [ ] Once we have more special test features, we may want to add
  non-determinism and control it when testing (would fix some looping cases)

CODE IMPROVEMENTS:
- [x] Better interfaces for `solution` and `database`?
  - [x] Solutions should maybe be sets, with the right equality
  - [x] Database abstraction to enable replacing with tries?
- [ ] `NONE` getting exposed to end user in solutions
  - [ ] At the very least we can make it so it doesn't get printed
  - [ ] Make sure that we don't have generated symbols either
  - [ ] We can probably turn this knob when we add more solution querying
- [ ] Maybe having an `attribute` struct would be better
  - [ ] Currently, lots of duplication when compiling attributes
- [ ] Testing infrastructure improvements

QUESTIONS FOR US:
- [ ] Is it useful to know what is a binding vs a reference in our AST?
- [ ] Reconsider `fact` and `make-fact` stuff?

QUESTIONS FOR OH:
- [x] How do we not thread around the symbol tables in annoying way
- [x] Binding structure for built-ins: how can we check what is a built-in
- [ ] `_` logic variables
  - [ ] More generally, is there any way for our compiler to generate
        fresh logic variables?
- [ ] Ask about structuring tests and test redundancy / duplication.
  - [ ] Testing expansion vs runtime vs etc
  - [ ] Also keep in mind we may want to export runtime functions
        and the AST separately from each other?
- [ ] Is there any more “compilation” that we could do, besides to AST?
- [ ] Is it even possible to do renaming through an import?

QUESTIONS (ONE DAY):
- [ ] Should things be `#:transparent`
  - Either implement `equal?` or a predicate + `check-satisfied`
  - For now it is okay to leak the implementation details
  - Or, can use an existential contract (which wraps in opaque lol)
