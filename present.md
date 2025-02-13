---
marp: true
theme: rose-pine
---

<!--
What is the purpose of the DSL you propose? What are the concepts in the domain,
how do they map to linguistic features, and what computation arises from them?

Examples of programs in your DSL.

Grammars (for macros) and signatures (for functions) for the syntax of your DSL,
together with brief purpose statements. If your DSL is large, you can present a
subset in-class and provide the full specification in your design document.

Implementation milestones, breaking down the work in a sensible order.
Depending on your DSL, it might make most sense to work bottom-up, starting
from runtime support and layering syntax on top. Or, it might make sense to
start from the syntax of your DSL and work down towards the runtime.
-->

<style>
  :root {
    font-family: "Helvetica Neue", Helvetica, Arial, sans-serif, system;
    font-size: 16pt;
  }

  h1 {
    font-size: 32pt;
    margin-bottom: 1rem;
  }

  .hljs-name,
  .hljs-attribute {
    color: var(--love);
  }
</style>

<!-- _class: lead -->

<style scoped>
  h1 {
    margin-bottom: 2rem;
  }

  .cite-container {
    position: absolute;
    max-width: min-content;
    bottom: 4em;
    font-size: 12pt;
    color: var(--text);
  }
</style>

# Finite-Choice Logic Programming

Ari Prakash, Zack Eisbach

<div class="cite-container">
  <b>Inspired by:</b>
  <cite>
    Chris Martens, Robert J. Simmons, and Michael Arntzenius. 2025.
    Finite-Choice Logic Programming. Proc. ACM Program. Lang. 9, POPL,
    Article 13 (January 2025), 29 pages.
    <a href="https://dl.acm.org/doi/pdf/10.1145/3704849">
      https://dl.acm.org/doi/pdf/10.1145/3704849
    </a>
  </cite>
</div>

---

# Logic Programming

- Many problems involve generating information based on some logical constraints.

- We also want to characterize the space of such possibilities.

- Logic programming is one way to describe such possiblity spaces.

  - Takes a declarative approach based on defining relations subject to constraints.

- Uses user-specified rules on encoded facts to make deductions.

---

# Datalog

Datalog (and its numerous variants) is a popular logic programming language. A Datalog program is characterized by the facts that can be deduced using the initial facts and supplied rules.

```datalog
parent(alice, bob).
parent(bob, carol).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

We deduce `ancestor(alice, bob)`, `ancestor(bob, carol)`,
and `ancestor(alice, carol)`.

---

# Answer Set Programming

Answer set programming (ASP) is another approach to logic programming that tries to compute _all_ possible deductions that could be made. It outputs all possible "universes" of choices where the rules held.

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

Generates all possible 3-colorings of the graph (as specified by the `edge` facts).

---

# Finite-Choice

Finite-choice logic programming generalizes both Datalog and ASP, emphasizing the role that _choice_ plays. In doing so, some implementation shortcomings of ASP can be avoided, and its semantics can be made clearer.

A finite-choice logic program also consists of not just facts and relations, but also _functional relations_ which relate their inputs to exactly one choice of output.

```dusa
terrain R is { mountain, forest, ocean } :- region R.
terrain R is { forest, ocean } :- adjacent R S, terrain S is ocean.
```

Ensures that each region `R` is assigned exactly one terrain type, and `ocean` is not adjacent to `mountain`.

---

# Finite-Choice

Using these functional relations, we can characterize infinitely large possibility spaces.

```dusa
#builtin NAT_SUCC s

run 0 is { stop, go }.
run (s N) is { stop, go } :- run N is go.
```

Each solution is a collection of facts of the form `run K is go`, for some `K < N` followed by exactly one `run N+1 is stop`. This possibility space is infinite, and can be sampled from.

We can add an additional constraint:

```dusa
#forbid run 10 is go.
```

This limits the possibility space to `N < 10` necessarily.

---

# Examples

Parent/Ancestor (from Datalog)

```lisp
(define-logic datalog
  (parent alice bob)
  (paernt bob carol)

  (:- (ancestor X Y) (parent X Y))
  (:- (ancestor X Y) (parent X Z) (ancestor Z Y)))
```

Graph coloring (from ASP)

```lisp
(define-logic asp
  (edge a b) #;(...)
  (:- (edge B A) (edge A B))
  (:- (node N) (edge N _))
  (:- (is (color N) (choice 1 2 3)) (node N))

  (:- (is? ok (choice #t)))
  (:- (is ok (choice #f)) (edge A B) (= (color A) (color B))))
```

---

# Grammar

We define a series of declarations under `define-logic` and obtain a query-able object to check the deductions that were made.

```bnf
(define-logic id <declaration> ...+)

<declaration> := (<conclusion>) ; facts
               | (:- <conclusion> <premise> ...+)

<premise> := <attribute>
           | (is <attribute> <term>)

<conclusion> := <attribute>
              | (is <attribute> <choices>)
              | (is? <attribute> <choices>)

<attribute> := <symbol>
             | (<symbol> <atomic-term> ...+)

;; opportunity for syntactic sugar later
<choices> := (choice <term> ...+)
```

---

# Milestones

1. Deeply understand FCLP paper and the exploration algorithm.

2. Design an adequate data representation, allowing data hiding.

3. Construct an embedding of the core language (functional predicates).

4. Implement as a language extension: macros, syntax, binding structure.

5. Extend with support for additional features, syntactic sugar, etc.

6. Consider opportunities for further interoperation with Racket.
