#lang racket

(provide (except-out (all-defined-out)
                     fact)
         (rename-out [make-fact fact]))

;; A [Maybe X] is one of:
;; - #f
;; - X
;; and represents a value that might not exist.

;; A [Option X] is one of:
;; - NONE
;; - X
;; and represents a value that might not exist, using a sentinel value
;; to differentiate between the NONE case and #f being present

(define NONE (gensym "none"))

;; none? : Any -> Bool
(define (none? x)
  (equal? NONE x))

;; A Datum is RacketDatum
;; Ideally, we would allow any Racket datum here, but this may change
;; if implementing this is infeasible.
;; CURRENTLY, it seems as if we will need to be able to compare for equality
;; to compare attributes and determine when conflicts arise

;; A Relation is one of
;; - Symbol, representing relation symbols, or
;; - Procedure, representing an imported relation

;; A Fact is a (fact Relation [ListOf Datum] [Option Datum]).
;; It represents a known fact (either given or deduced) in the database.
;; Currently, stored facts will always have Symbols, since imported
;; relations are computed repeatedly and not cached.

;; name + terms = "attribute"
;; if we need to use attributes without values, we can abstract
(struct fact [rel terms value] #:transparent)

(define (make-fact rel terms [value NONE])
  (fact rel terms value))

;; A Variable is a (variable Symbol)
(struct variable [name] #:transparent)

;; A Term is one of:
;; - Datum
;; - Variable

;; An OpenFact is a (fact Symbol [ListOf Term] [Option Term])
;; It represents a fact that has not yet been fully grounded.

;; A Constraint is a (constraint Symbol [ListOf Datum] [ListOf Datum])
;; It represents a constraint on any potnetial fact that would look like
;; this one, where the values cannot be any of the ones in the value list.
(struct constraint [rel terms none-of])

;; A RuleFragment is (rule-frag Symbol [ListOf Term] [ListOf Term] Boolean)
;; It is an _internal representation_ of the source syntax, which in
;; general may contain variables. These can be combined to form rules
;; or facts (closed rules with no premises).
;; Notably, a Term is an immediate, and does not have nested terms:
;; this is a difference from the surface <term> syntax; these nested
;; terms will be desugared in an ANF-like pass.

;; A ClosedRuleFragment is
;; (rule-frag Symbol [ListOf Datum] [ListOf Datum] Boolean)
;; it represents a rule fragment where all variables have been substituted
;; for data. Note that a RuleFragment is "open by default", while in contrast
;; a Fact is "closed by default", hence the inconsistent naming convention

(struct rule-frag [name terms choices is??] #:transparent)

;; A Rule is a (rule RuleFragment [ListOf OpenFact])
;; It represents a conclusion which follows from the (0 or more) premises
;; All occurrences of variables in the conclusion must be bound by
;; the premises. A conclusion with no premises is just a fact.

(struct rule [conclusion premises] #:transparent)

;; define-logic binds the identifier to a `Logic`
;; `Logic` is an object that can be used to obtain solutions.

;; Internally, we know that a Logic is a (logic [ListOf Rule] [ListOf Rule])
;; storing the rules which never require making a choice in the conclusion
;; and the rules which require making choices (> 1 option on RHS of `is`)
;; TODO: add in information about imported Racket function(al relation)s

(struct program [deduce-rules choose-rules] #:transparent)

;; A Solution is an opaque object that can be queried for propositions.

;; For now (naively), a Solution is a (solution Database)
;; (where Database is documented in "database.rkt", which we require)

(struct solution [database] #:transparent)
