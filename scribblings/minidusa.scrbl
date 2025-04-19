#lang scribble/manual

@(require scribble/example
          (for-syntax racket/base)
          (for-label racket minidusa))

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
taught by Michael Ballantyne.

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
@codeblock{
 (logic
 (parent 'alice 'bob)
 (parent 'bob 'carol)

 ((ancestor X Y) :- (parent X Y))
 ((ancestor X Y) :- (parent X Z) (ancestor Z Y)))
}

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

@codeblock{
 (logic
 ; 3 possible colors, each node has exactly one
 (((color X) is {1 2 3}) :- (node X))
 ; condition to avoid
 ((ok) is {#t})
 (((ok) is {#f}) :- (edge X Y)
 ((color X) is C)
 ((color Y) is C))
 ; facts (we will see more concise ways of writing this later)
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
}

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
 declarations with no extra effect, much like @code{begin}, this is especially
 useful for writing @(seclink "extensions" "Extending") on top of miniDusa, as
 macros cannot expand out into @italic{multiple} s-expressions.}

@section{Imports}

@section{Extending}

@section{API}

@defproc[(all [program program?]) (stream? solution?)]{
Obtain a stream of all possible solutions of the given program.
The stream may be infinite, and computing the next item may not
always terminate.
}
