#lang scribble/manual

@(require scribble/example minidusa syntax-spec-v3
          (for-syntax racket/base)
          (for-label racket minidusa syntax-spec-v3))

@(define eval (make-base-eval '(require racket minidusa)))

@title{miniDusa}
@author{Ari Prakash and Zack Eisbach}

@defmodule[minidusa]

miniDusa is a finite-choice logic programming DSL within Racket similar to
@(hyperlink "https://dusa.rocks/docs/" "Dusa"). We implement the fact-set
semantics as described by the
@(hyperlink "https://dl.acm.org/doi/pdf/10.1145/3704849"
            "Finite-Choice Logic Programming") paper by Chris Martens, et al.
This DSL was implemented as our final project for Northeastern University's
@(hyperlink "https://mballantyne.net/hyol/" "CS 3620 (Hack Your Own Language)")
taught by Michael Ballantyne during Spring 2025.

The DSL is implemented as a macro to specify the program itself, and runtime
functions to solve it and query the results.

@section{Motivation}

@;TODO

@section{Purpose and Concepts}

Datalog (and variants) is a popular logic programming language characterized by
writing facts, and rules that can deduce new facts if their premises are
satisfied. Here is an example Datalog program and its translation into miniDusa:

@verbatim{
 parent(alice, bob).
 parent(bob, carol).

 ancestor(X, Y) :- parent(X, Y).
 ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
}
@racketblock[
 (logic
   (parent 'alice 'bob)
   (parent 'bob 'carol)

   ((ancestor X Y) :- (parent X Y))
   ((ancestor X Y) :- (parent X Z) (ancestor Z Y)))
 ]

Datalog-style languages deduce the smallest collection of assertions that can be
deduced from the starting facts that are closed under the specified rules.

Another approach to logic programming is called answer-set programming, and it
tries to compute @italic{all} possible deductions, meaning that answer set
programs can evaluate to @italic{multiple} collections of assertions (each one
known as a solution). miniDusa is also capable of emulating answer-set
programming, here is an example written in
@(hyperlink "https://potassco.org/clingo/" "clingo") and its translation:

@verbatim{
 % define 3 possible colors where each node can have exactly one
 { color(X,1..3) } = 1 :- node(X).

 % condition to avoid
 :- edge(X,Y), color(X,C), color(Y,C).

 % Facts
 node(1..6).
 edge(1,(2;3;4)).
 edge(2,(4;5;6)).
 edge(3,(1;4;5)).
 edge(4,(1;2)).
 edge(5,(3;4;6)).
 edge(6,(2;3;5)).
}

@racketblock[
 (logic
   (code:comment "3 possible colors, each node has exactly one")
   (((color X) is {1 2 3}) :- (node X))
   (code:comment "condition to avoid")
   ((ok) is {#t})
   (((ok) is {#f}) :- (edge X Y)
                   ((color X) is C)
                   ((color Y) is C))
   (code:comment "facts (we will see more concise ways of writing this later)")
   (edge 1 2)
   (edge 1 3)
   (edge 1 4)
   (edge 2 4)
   (edge 2 5)
   (edge 2 6)
   (edge 3 4)
   (edge 3 5)
   (edge 5 4)
   (edge 5 6)
   (edge 6 3)
   ((node X) :- (edge X _))
   ((node X) :- (edge _ X)))
 ]

Finite-choice logic programming generalizes both Datalog and ASP,
emphasizing the role that choice plays. In doing so, the grounding
step of ASP can be avoided, and the semantics can be made clearer.
Finite-choice logic programming is exemplified through the Dusa programming
language.

A finite-choice logic program also consists of not just facts and
relations, but also functional relations which relate their inputs
to exactly one choice of output.

@section{Syntax}

@defform*[#:literals (:- is decls)
          ((logic decl ...)
           (logic #:import (import ...) decl ...))
          #:grammar
          [(decl conclusion
                 (conclusion :- premise ...+)
                 (decls decl ...))
           (import fn-id
                   [id fn-expr])
           (conclusion attribute
                       (attribute is {term ...+}))
           (premise attribute
                    (attribute is term))
           (attribute (id term ...))
           (term id
                 number
                 boolean
                 string
                 symbol
                 char)]
          #:contracts
          [(fn-id procedure?)
           (fn-expr procedure?)]]{
 The @code{logic} macro is the main entry point into miniDusa, and has two
 variants: with or without @(seclink "Imports" "imports").

 Just like Dusa, our program is comprised of a list of @tt{decl}s that that
 are either base facts or rules that fire based on premises (in the case with
 @tt{:-}). We also inclde a @tt{decls} form that can contain a block of
 declarations with no extra effect, much like @code{begin}; this is especially
 useful for writing @seclink["Extending" "extensions"] on top of miniDusa, as
 macros cannot expand out into @italic{multiple} s-expressions.}

@section{Imports}

miniDusa allows importing arbitrary Racket functions as relations to be used
within the program. We use the @racket[#:import] keyword to specify what values
from the surrounding environment ought to be visible inside the miniDusa
environment.

@racketblock[
 (logic #:import ([p +])
   ((foo) :- ((p 1 2) is 3))
   (code:comment
    "arity checking is disabled for imports as they can be variadic")
   ((bar X) :- ((p 1 2 3) is X)))]

The previous example imports @racket[+] as the identifier @racket[p]. We are
allowed to apply it to arguments (either literal or @italic{alteady bound}
variables) and potentially bind the result. The return value is checked for
equality with the expected value, or bound as a variable.

@racketblock[
 (code:comment "note the shorthand for not renaming an import")
 (logic #:import (add1)
   (foo 1)
   ((bar) :- (foo X) ((add1 X) is 2)))]

This example binds @racket[X] @italic{before} applying @racket[add1] to it.
Since the value of all arguments are known at the time of application, miniDusa
allows this rule. It will also check that the value returned is indeed
@racket[2] in order to deduce the conclusion.

Unlike Dusa, we disallow running imported relations
@hyperlink["https://dusa.rocks/docs/language/builtin/#other-built-in-relations"
           "backwards"]. Running a builtin backwards means @italic{binding} a
new logic variable at one of the argument positions of an imported relation. In
this case, we will not have all the arguments ready to apply, and need to
somehow find what argument value will result in the function returning what we
want. This is impossible in general, and so we disallow this behavior entirely.

Imported functions are expected to be well-behaved: they should be pure and
their return values should be able to be checked for equality using
@racket[equal?].

@section{Extending}

We provide a @tt{logic-macro}
@seclink["Extension_classes"
         "extension class"
         #:doc '(lib "syntax-spec-v3/scribblings/main.scrbl")]
to allow extending the core
miniDusa syntax using @code{define-dsl-syntax} and the power of Racket's macro
system. We allow macros to expand into more than one declaration by way of the
@tt{decls} block. We can define a macro similar to Dusa's
@(hyperlink "https://dusa.rocks/docs/language/constraints/#forbid-constraints"
            "forbid constraints") as follows:

@racketblock[
 (define-dsl-syntax forbid logic-macro
   (lambda (stx)
     (syntax-parse stx
       [(_ name p ...+)
        #'(decls ((name) is {#t})
                 (((name) is {#f}) :- p ...))])))

 (logic #:import ([s add1])
   ((run 0) is {'stop 'go})
   (((run M) is {'stop 'go})
    :- ((run N) is 'go) ((s N) is M))

   (forbid ok ((run 10) is 'go)))
 ]

@defidform[logic-macro]{
 The macro extension class for miniDusa macros that expand into one or more
 declarations (rules or facts).
}

A limitation that miniDusa macros currently have is that macros are not truly
hygenic, as our runtime representation uses raw symbols or procedures as our
relation names. Therefore, macros have to be careful about the way they deal
with generating new relations: in the example above, the macro took a @tt{name}
from the user since we do not know what other names might be used already.

@section{API}

@defproc[(all [program program?]) (stream? solution?)]{
 Obtain a stream of all possible solutions of the given program.
 The stream may be infinite, and computing the next item may not
 always terminate.
}
