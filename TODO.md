BEFORE WEDNESDAY:
- [ ] Figure out binding structure
- [ ] Write simple compiler
- [ ] Prep for code walk

NICE TO HAVE:
- [ ] `:-` macro

NEXT STEPS:
- [ ] Static check for symbol arity (and choose)
- [ ] Better interfaces for `solution` and `database`
- [ ] Implement `choose`


QUESTIONS:
- Michael's hack from Piazza:
  - Can this hack work across non-terminals? In case we wanted to
    have some syntax restrictions on where just `bind` can appear
- Is there `racket-datum` or similar? What about `symbol`?
  - Any advice on having `expr` but not having everything get parsed as that?
    Maybe this will be easier once we actually compile to things