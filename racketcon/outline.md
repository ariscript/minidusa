First talk about miniDusa as Dusa
- intro to finite-choice logic programming via examples
  - motivation! partially through example but also re-state Dusa goals
  - but NOT "this is datalog and this is ASP ..."
- ideally find examples which will be useful to revisit later

How miniDusa is implemented as a hosted DSL + syntax-spec
- the pipeline
- an emphasis here should be about how syntax-spec makes
  things easy, and maybe some places where we had to hack
  - IDE support is useful to mention, could give renaming demo
  - don't give a syntax-spec tutorial but explain what it gets for us
    - ex: not "binds and refs" but we can still explain how the binding
      structure is dissimilar from FP but syntax-spec still works (mostly)
- Q: what order should this be happening in?
  - perhaps interleave explaining how the extension works and giving examples?
    - possibly (not sure) could talk about {core minidusa} ->... and THEN
      talk about how the surface gets extended with macros and elaborated

Extensions
- macros
  - abstracting over encoding of data
  - hopefully can revisit existing examples from first part
- interacting with Racket data
  - imported functions
  - external relations

Classic examples:
- graph reachability -> use a macro
- graph coloring for choice -> macro
- graph reachability / computation -> get data externally

Example notes:
- external relation name passed to a macro, which would let the dusa
  add some facts there (that doesn't seem so useful though)
  - the interesting thing is computing more facts, not just adding
    (like not just "oh we add these nodes/edges to a graph")
  - Q: what would an example of meaningful computation be on this?
  - if the computation is annoying to do in Racket, we might want to
    do with Dusa. But why would someone want a macro? Seems useless
- external relations to bring in Racket data as values of facts, plus
  imported functions to compute on those values, from Racket
  - this seems more useful but depends on the structure of the data
    that is brought in from Racket
- we can pretty easily emulate embedded Datalog stuff, but we want
  to show off the choice part as well!
  - ex: we could do the terrain generation example and have it be integrated
  - Racket has first-class support for images, and we can choose among them
  - moral for making example: choosing is great for creativity and 
    images are also great for creativity (this is why chris is so cool)

```minidusa/foobar
(define forest (make-img ...))
grid N M is {(rkt forest) (rkt water) (rkt mountain)}
forbid grid N M is X and == X (rkt forest) is #t
```

More implementation details?

Maybe "future work"??

A goal:
- make sure that everyone walks away understanding things,
  even if some people want more
  - we can talk to people about some of these hacks
- chances are, initially we might have too many details
  -> can always go into overflow slides or something

Some more goals:
- people should understand fclp and want to try (mini)Dusa
- people should be intrigued by syntax-spec
  - Michael will do more of this but we can start gaining interest
- ... what can people learn about macros, extensions, etc?
  - What story do we want to tell here?


Rough time distribution (25m):
- intro to dusa
- our structure
- 
