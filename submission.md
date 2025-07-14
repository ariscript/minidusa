Venue options:
- RacketCon
  - Call for abstracts: July 15th
  - October 4-5, 2025
  - Idea: just a fun place to go and talk about Racket stuff
  - This can easily be done with other things
- Scheme Workshop
  - Call for submissions: July 17th
  - October 16th, 2025 (Singapore, ICFP/Splash)
  - Full papers and experience reports: 5-12 pages
- IFL
  - Call for "normal papers": June 16, 2025
  - Call for draft papers: August 4, 2025
  - October 1-3, 2025
  - Actual paper submission: December 15, 2025
- TFP
  - Call for normal papers: mid November
  - Call for draft papers: mid December
  - Workshop: mid January
  - Actual submission: mid February
- ICFP Experience Report
  - 12 pages
  - Call for papers: late February

Question:
- If we did Scheme Workshop, how would that conflict with
  other submission opportunities?
  - Maybe if you aren't in the proceedings its okay?
  - Cameron probably knows this answer


## Meeting Notes 5.21.25

Michael wasn't sure exactly what the merits of something like Scheme Workshop
vs ICFP Experience Report were, but Matthias later had more thoughts
- Could write something for workshop if it is non-archival, but that runs the
  risk of not having enough differentiating. Could also just give a talk at the
  workshop with no corresponding proceedings to get feedback
- We can spend a few weeks working / trying to figure things out, then reassess

Potential contributions / talking points: 

1. Easier to write (especially on the front end)
- If we had full feature parity, we could compare LOC
- This is more of a `syntax-spec` story, not necessarily unique to this impl
- Demonstrates how `syntax-spec` works with syntax that isn't so Racket-like
- IDE support, like binding arrows and rename refactoring (with much less code)

2. Extends Dusa with macros, enabling more domain-specific programs
- The macros that implement features that are a part of the language (like
  `#demand` and `#forbid` stuff, perhaps `#lazy`/etc) are less interesting,
  since this is just a (perhaps convenient) implementation strategy
- Enabling users to write their own macros to model data better is a much more
  compelling story: abstracts over how data is being encoded into the solver

3. Extends Dusa with better interoperability with the host (Racket)
- We support arbitrary builtins; Dusa could too but that is non-trivial
- There is a lot of opportunity for exploration here: what can now be expressed
  that was hard/impossible before?

Our current implementation already has a _bit_ in each of these regards, but
there is room to make things a lot more fleshed out (especially point 3).
Remember, things should be macros _for a reason_---using macros in it of itself
is not a meaningful contribution.

## Meeting Notes 7.7.25

Macros, small core and elaborating things down is something people care about.
Our work should aim to be useful to people even who won't ever use these tools.
- Common interests include language-oriented programming, macros, DSLs, or even
  design of language extension systems

We should emphasize the things we inherit "for free" from Racket, and note that
this is part of syntax-spec and not unique to our implementation
- Claim: getting these things for free is useful (and getting these
  abstractions for free is useful for general DSLs!)
