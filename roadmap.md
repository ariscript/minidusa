Things we need to do:
- Allow arbitrary expressions (maybe with syntactic boundary for parsing)
- See `TODO.md` for more fixes and clean-ups
  - Maybe some stuff from there should be moved here...

- Better integration with Racket, like displaying / manipulating solutions
  - Example: drawing images on a grid, like `cell X Y is {castle, ...}`
  - More interesting: what is the right API for doing this?
- `#lazy`, program transformation implemented as a macro
  - Maybe this generates fresh relation names which should work
- Transformation to allow nesting
  - Maybe this is a spec sitting on top of our spec, or maybe a mere macro
  - This probably depends on the implementation of the following...
    - This is a Michael question
- Make generating fresh relation names better!
  - It would be cool if you could use Racket's hygiene and not `gensym`
  - This would require changing the `syntax-spec` in some way
  - This is a Michael question: two specs? spec + macro?
    - This is actually interesting!
- Open rules, `is?`
  - Then, the only Dusa features unsupported are running things backwards
- Stretch goal: parse Dusa test cases and benchmark / compare
  - Then again, `mini` absolves us of these issues
  - This would be more useful if we plugged in a faster solver
    (which would have to call back into Racket for imports)
