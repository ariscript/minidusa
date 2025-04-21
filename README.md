## miniDusa

miniDusa is a finite-choice logic programming DSL within Racket similar to
[Dusa](https://dusa.rocks/). We implement the fact-set semantics as described in the
[Finite-Choice Logic Programming](https://dl.acm.org/doi/pdf/10.1145/3704849)
paper by Chris Martens, Rob Simmons, and Michael Arntzenius.
This DSL was implemented as our final project for
Northeastern University’s [CS 3620 (Hack Your Own Language)](https://mballantyne.net/hyol/)
taught by Michael Ballantyne during Spring 2025.

Here is an example program whose solutions are 3-colorings of a graph.
The graph is tersely specified using an extension to miniDusa, which is
enabled by [syntax-spec](https://docs.racket-lang.org/syntax-spec-v3/index.html).

```racket
#lang racket

(require minidusa)

(define-dsl-syntax graph logic-macro ...)

(define-dsl-syntax forbid logic-macro ...)

(logic
  ;; graph macro allows graph specification using adjacency list notation
  (graph edge
          ('a ['b 'c 'e])
          ('c ['b 'd]))
  ((node X) :- (edge X _))
  (((color X) is {1 2 3}) :- (node X))

  ;; if the following condition is met, the solution is rejected
  ;; this ensures that solutions are valid 3-colorings
  (forbid ok
          (edge X Y)
          ((color X) is C)
          ((color Y) is C)))
```

The following miniDusa example demonstrates how finite choice may be used in
order to conveniently generate characters and backstories for creative purposes.



## Installing and running

Check out this Git repository, change directory into it, and run:

<!-- once we get this on Racket packages, this needs to be changed -->
```
raco pkg install
```

Then import as

```
(require minidusa)
```

Once installed, you can access the documentation via:

```
raco docs minidusa
```

Finally, you can run the tests with:

```
raco test -p minidusa
```