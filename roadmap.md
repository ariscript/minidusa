In progress:
- `rel-var` appearing in Racket expressions is not allowed, which is preventing
  us from adding in import sugar
- If we keep syntax objects around at runtime and return them in solutions, how
  are supposed to write any tests? Should this be exposed to end users? If not,
  how will _they_ disambiguate?
  - We also have to actually keep the syntax objects around, and compare with
    the right notions of equality (`bound-identifier=?` apparently)
- Better motivating examples for macro hygeine

Direct next steps:
- Open rules, `is?`
  - Then, the only Dusa features unsupported are running things backwards
- Allow arbitrary expressions (maybe with syntactic boundary for parsing)
- See `TODO.md` for more fixes and clean-ups
  - Maybe some stuff from there should be moved here...

- Better integration with Racket, like displaying / manipulating solutions
  - Example: drawing images on a grid, like `cell X Y is {castle, ...}`
  - More interesting: what is the right API for doing this?
    - Maybe better ways to iterate over facts (like `join`s)
- `#lazy`, program transformation implemented as a macro
  - Maybe this generates fresh relation names which should work
  - This could be cool, but isn't necessarily key that it's a macro

- Transformation to allow nesting
  - This is apparently a non-local change (think about the structure of the
    spec itself; apparently drilling down wouldn't work either)
- Maybe there are questions about whether to allow macros in more positions,
  like in logic variable positions. Think about logic variable freshness and
  whether that _actually_ comes up without this?
  - Currently, we get nothing from fresh logic variables. Allowing macro
    expansion on RHS of `:-`s could make this actually work

- Parse Dusa test cases and benchmark / compare
  - Then again, `mini` absolves us of these issues
  - This would be more useful if we plugged in a faster solver
    (which would have to call back into Racket for imports)
  - Gives a better story for implementation burden / LOC
    - Though we may also want more of their techniques for this too...

- Better integration with Racket
  - Some contract-y questions about how to make things safer. What are the
    right restrictions to have?
    - What about higher-order stuff? Should we have a first-order restriction?
  - Better interface to be able to run things backwards (maybe providing a
    function that produces possible inputs, given an output?)
    - Check in on how Dusa actually implements this
  - Think about how to best interact with host language data, or perhaps other
    software, like Michael's example (Fig 3 in ICFP pearl) with SQL query stuff
  - These can _then_ be wrapped up as macros, like above
  - Potential for Racket subexpressions with free variables in a boundary form
    (Michael has a little bit about this in his pearl)
    - This really only makes sense for running forward, at least


Resources:
- As motivation, we should be looking at Rob's
  [Advent of Dusa](https://typesafety.net/rob/blog/advent-of-dusa-2024) to see
  where he had to bail out to JavaScript, or use some encoding hacks
- [Michael's ICFP Pearl](https://dl.acm.org/doi/pdf/10.1145/3674627)
- [BYODS](https://dl.acm.org/doi/pdf/10.1145/3622840)
  - Lots of clear connections to bringing in data from a host language
- [Ascent](https://s-arash.github.io/ascent/cc22main-p95-seamless-deductive-inference-via-macros.pdf)
  - Logic programming implemented as a procedural Rust macro
- [Formulog](https://dl.acm.org/doi/pdf/10.1145/3428209)
  - Logic programming + some functional programming stuff
