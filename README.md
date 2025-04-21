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

```racket
(define story-program
  (logic #:import ([!= (compose not equal?)]
                  string-append)
    ((character 'hero) is {"Zack" "Ari" "Ben" "Michael"})
    ((character 'sidekick) is {"Zack" "Ari" "Ben" "Michael"})
    ((character 'villain) is {"Zack" "Ari" "Ben" "Michael"})

    ;; two characters with the same name must be the same character
    (forbid unique-roles ((character Char1) is X)
                         ((character Char2) is X)
                         ((!= Char1 Char2) is #t))

    ;; everyone must have a different job
    (((job C) is {"student" "TA" "prof" "unemployed"}) :- ((character C) is _))
    (forbid unique-job ((job Char1) is X)
                       ((job Char2) is X)
                       ((!= Char1 Char2) is #t))

    ;; hero and villain cannot be from the same home
    (((home C) is {"CT" "NY" "MA"}) :- ((character C) is _))
    (forbid backstory ((home 'hero) is X)
                      ((home 'villain) is X))

    (((story) is {Result}) :-
                           ((character 'hero) is Hero)
                           ((character 'sidekick) is Sidekick)
                           ((character 'villain) is Villain)
                           ((job 'hero) is HeroJob)
                           ((job 'villain) is VillainJob)
                           ((home 'hero) is HeroHome)
                           ((home 'villain) is VillainHome)
                           ((string-append "The heroic "
                                           HeroJob
                                           " "
                                           Hero
                                           " from "
                                           HeroHome
                                           " worked with "
                                           Sidekick
                                           " to thwart the wicked "
                                           VillainJob
                                           " "
                                           Villain
                                           " of "
                                           VillainHome) is Result))))

;; we generate lots of story strings; we query the first such string below
(define solution-stream (all story-program))

> (get (stream-first solution-stream) 'story)
"The heroic TA Zack from CT worked with Ari to thwart the wicked prof Ben of NY"
```


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