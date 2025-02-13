## Purpose and Concepts

Logic programming is a declarative programming paradigm where logical
deductions are made from collections of facts and rules. In addition
to being well-suited for inherently logical tasks, logic programming
can be used to effectively characterize spaces of possibilities subject
to some constraints.

### Datalog

Datalog (and its numerous variants) is a popular logic programming language.
A Datalog program is characterized by the facts that can be deduced using the
initial facts and supplied rules.

```datalog
parent(alice, bob).
parent(bob, carol).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

We deduce `ancestor(alice, bob)`, `ancestor(bob, carol)`,
and `ancestor(alice, carol)`.
This is the smallest collection of assertions that can be obtained starting
from the initial facts that is closed under the inference rules for `ancestor`.

### Answer Set Programming

Answer set programming (ASP) is another approach to logic programming
that tries to compute _all_ possible deductions that could be made.
Unlike Datalog above, where the meaning of a program is given by a
single collection of assertions, the meaning of an answer set program
is _potentially multiple_ collections of assertions.

```clingo
% define 3 possible colors where each node can have exactly one
{ color(X,1..3) } = 1 :- node(X).

% condition to avoid
:- edge(X,Y), color(X,C), color(Y,C).

% Facts
node(1..6).

edge(1,(2;3;4)).  edge(2,(4;5;6)).  edge(3,(1;4;5)).
edge(4,(1;2)).    edge(5,(3;4;6)).  edge(6,(2;3;5)).
```

This program generates all possible 3-colorings of the graph
(as specified by the `edge` facts).

Answer set programming languages are almost ubiquitously implemented
using two steps---first, programs with variables are _grounded_,
producing logic programs with negation but no variables. Then, these
programs are passed to a _solver_.

### Finite-Choice Logic Programming

Finite-choice logic programming generalizes both Datalog and ASP,
emphasizing the role that _choice_ plays. In doing so, the grounding
step of ASP can be avoided, and the semantics can be made clearer.
Finite-choice logic programming is exemplified through the
[Dusa](https://dusa.rocks) programming language.

A finite-choice logic program also consists of not just facts and
relations, but also _functional relations_ which relate their inputs
to exactly one choice of output.

```dusa
terrain R is { mountain, forest, ocean } :- region R.
terrain R is { forest, ocean } :- adjacent R S, terrain S is ocean.
```

This program snipped ensures that each region `R` is assigned exactly
one terrain type, and that `mountain`s are not adjacent to `ocean`s.

Using these functional relations, we can characterize infinitely
large possibility spaces:

```dusa
#builtin NAT_SUCC s

run 0 is { stop, go }.
run (s N) is { stop, go } :- run N is go.
```

Each solution is a collection of facts of the form `run K is go`,
for some `K < N` followed by exactly one `run N+1 is stop`.
This possibility space is infinite, and can be sampled from.

We can add an additional `#forbid` constraint to limit
the possibility space to `N < 10` necessarily:

```dusa
#forbid run 10 is go.
```

## Example Programs

We can rewrite some of the examples seen above in our DSL,
demonstrating how finite-choice logic programming generalizes
Datalog and ASP.

### Simple Logic Programming: Ancestor Relation

```lisp
(define-logic datalog
  (parent 'alice 'bob)
  (parent 'bob 'carol)

  (:- (ancestor x y) (parent x y))
  (:- (ancestor x y) (parent x z) (ancestor z y)))
```

### Answer Set Programming: Graph Coloring

```lisp
(define-logic asp
  (edge 'a 'b) #;(...)
  (:- (edge b a) (edge a b)) ; note that a and b are not 'a and 'b
  (:- (node n) (edge n _))
  (:- (is (color n) (choice 1 2 3)) (node n))

  ;; the `ok` relation is the desugaring of `forbid`
  (:- (is ok (choice #t)))
  (:- (is ok (choice #f)) (edge a b) (= (color a) (color b))))
```

### Finite Choice: Filtering Infinite Spaces

```lisp
(define-logic stop-go #:import [add1]
    (is (run 0) (choice 'stop 'go))
    (:- (is (run (add1 n)) (choice 'stop 'go)) (is (run n) 'go))

    (forbid (is (run 10) 'go)))
```

## Grammars and Signatures

### Grammar

```bnf
(define-logic id <declaration> ...+)
(define-logic id #:import [id ...+] <declaration> ...+)

<declaration> := (<conclusion>) ; facts
               | (:- <conclusion> <premise> ...+)

<premise> := <attribute>
           | (is <attribute> <term>)

<conclusion> := <attribute>
              | (is <attribute> <choices>)

<attribute> := <symbol>
             | (<symbol> <atomic-term> ...+)

;; opportunity for syntactic sugar later
<choices> := (choice <term> ...+)

<term> := <atomic-term>
        | (<symbol> <atomic-term>...+)

<atomic-term> := <expr> ; any racket expression
```

We intend for some static checks to be in place:

- Identifiers in the conclusion of rules must be bound by premises
- Non-imported function symbols must have consistent arity
- Imported functional relations must only occur in positions where
  the functions are ``ran forward'', meaning the inputs to the function
  are known, not just outputs.

### Runtime Functions

```scheme
;; A [Maybe X] is one of:
;; - #f
;; - X
;; and represents a value that might not exist.

;; A Term is Any
;; Ideally, we would allow any Racket expression here, but this may change
;; if implementing this is infeasible.

;; A Fact is a (fact Symbol [ListOf Term]).
;; It represents a fact (either given or deduced) in the database.

;; define-logic binds the identifier to a `Logic`
;; `Logic` is an opaque object that can be used to obtain solutions.

;; A Solution is an opaque object that can be queried for propositions.

;; sample: Logic -> [Maybe Solution]
;; Obtain one possible solution of the given program, if one exists.

;; all: Logic -> [StreamOf Solution]
;; Obtain a stream of all possible solutions of the given program.
;; The stream may be infinite, and computing the next item may not
;; always terminate.

;; has: Solution Symbol Term ... -> Bool
;; Returns `#t` if the given proposition exists in this solution.
;; Raises an error if the number of arguments provided does not match the
;; number of arguments defined in the original relation.

;; get: Solution Symbol Term ... -> Term
;; Returns the value associated with the given proposition in this
;; solution.
;; Raises an error if the number of arguments provided does not match the
;; number of arguments defined in the original relation.

;; lookup: Solution Symbol Term ... -> [ListOf [ListOf Term]]
;; Query the solution for a proposition of the form provided. Providing
;; fewer arguments than the original definition results in the rest being
;; treated as wildcards, and included in the inner lists.
;; If provided with _more_ arguments than the original definition, `lookup`
;; raises an error.

;; facts: Solution -> [ListOf Fact]
;; Return a list of all known facts in this solution.
```

## Milestones

0. Deeply understand the original finite-choice logic programming
   paper and the exploration algorithm.

- There are two different semantics given in the paper: a _fact-set semantics_
  and a _fixed-point semantics_, which agree. There is also an algorithmic
  semantics which is closer in nature to the fixed-point semantics.
- While the Dusa implementation uses the algorithmic semantics described in
  the paper for efficiency, we intend to implement the fact-set semantics,
  as suggested by the implementors of Dusa, who we spoke with recently.

1. Design adequate internal data representations.

- These representations must be able to support the various runtime queries
  described above as a part of the language embedding.
- This must be able to support an iterative search with backtracking to
  explore / sample the possibility space.

2. Construct an embedding of the core language.

- Here, we want to strip down the features to their bare essentials and
  implement only runtime functionality, in the vein of µKanren.
- This implementation will likely be naïve, but will be able to stand
  as the embedded backend as we write macros.

3. Implement as a language extension: macros, syntax, binding structure.

- We will build off of the simple core, with macros informed by the initial
  language embedding described above. We will also implement relevant
  static checks, as described in the Syntax section.

4. Extend with support for additional features, syntactic sugar, etc.

- Here is where we can add additional syntactic sugar and perform program
  transformations to make it easier to write interesting programs

5. Consider opportunities for further interoperation with Racket.

- This is a somewhat novel section of our implementation: we will need to
  consider what types of Racket expressions are appropriate to be present
  in our logic programs.
- There are interesting questions to be considered involving functions,
  effects, and the amount of opacity to use when manipulating values.
  We _hope_ that a term can be `Any`, but we may need some more restrictions.

6. Reach goal: further optimizations and language extensions

- If we have time, we may want to consider an alternative backend based on
  the algorithmic semantics described in the Dusa paper, which uses more
  advanced data structures to perform an iterated search
- We may extend the language with _open rules_, as implemented in Dusa,
  which come with additional implementation complexities.
- We may be able to support running _some_ imported functions "backwards",
  which permits more natural use (as implemented in Dusa).

## References

Check out the Dusa [documentation](https://dusa.rocks/docs/?),
[source code](https://github.com/robsimmons/dusa),
and [paper](https://popl25.sigplan.org/details/POPL-2025-popl-research-papers/13/Finite-Choice-Logic-Programming).
