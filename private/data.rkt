#lang racket

(provide (except-out (all-defined-out)
                     fact)
         (rename-out [make-fact fact]))

(require racket/hash-code)

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
;; These will be compared for equality using `equal?`

;; A Relation is one of
;; - Syntax, representing (hygenic) relation names, or
;; - Procedure, representing an imported relation

;; A Fact is a (fact Relation [ListOf Datum] [Option Datum]).
;; It represents a known fact (either given or deduced) in the database.
;; Currently, stored facts will never store a procedure for the relation,
;; since imported relations are computed repeatedly and not cached.

;; name + terms = "attribute"
;; if we need to use attributes without values, we can abstract
(struct fact [rel terms value] #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc self other rec)
     (and (or (equal? (fact-rel self) (fact-rel other))  ; phys equal for procs
              (bound-identifier=? (fact-rel self) (fact-rel other)))
          (rec (fact-terms self) (fact-terms other))
          (rec (fact-value self) (fact-value other))))
   (define (hash-proc self rec)
     (hash-code-combine (eq-hash-code struct:fact)
                        (rec (fact-terms self))
                        (rec (fact-value self))))
   (define (hash2-proc self rec)
     (hash-code-combine (eq-hash-code struct:fact)
                        (rec (fact-terms self))
                        (rec (fact-value self))))])

(define (make-fact rel terms [value NONE])
  (fact rel terms value))

;; A Variable is a (variable Symbol)
(struct variable [name] #:transparent)

;; A Term is one of:
;; - Datum
;; - Variable

;; An OpenFact is a (fact Syntax [ListOf Term] [Option Term])
;; It represents a fact that has not yet been fully grounded.

;; A Constraint is a (constraint Syntax [ListOf Datum] [NEList Datum])
;; It represents a constraint on any potnetial fact that would look like
;; this one, where the values cannot be any of the ones in the value list.
(struct constraint [rel terms none-of]
  #:methods gen:equal+hash
  [(define (equal-proc self other rec)
     (and (bound-identifier=? (constraint-rel self) (constraint-rel other))
          (rec (constraint-terms self) (constraint-terms other))
          (rec (constraint-none-of self) (constraint-none-of other))))
   (define (hash-proc self rec)
     (hash-code-combine (eq-hash-code struct:constraint)
                        (rec (constraint-terms self))
                        (rec (constraint-none-of self))))
   (define (hash2-proc self rec)
     (hash-code-combine (eq-hash-code struct:constraint)
                        (rec (constraint-terms self))
                        (rec (constraint-none-of self))))])

;; A RuleFragment is (rule-frag Syntax [ListOf Term] [ListOf Term] Boolean)
;; It is an _internal representation_ of the source syntax, which in
;; general may contain variables. These can be combined to form rules
;; or facts (closed rules with no premises).
;; Notably, a Term is an immediate, and does not have nested terms:
;; this is a difference from the surface <term> syntax; these nested
;; terms will be desugared in an ANF-like pass.

;; A ClosedRuleFragment is
;; (rule-frag Syntax [ListOf Datum] [ListOf Datum] [OneOf Boolean 'tried])
;; it represents a rule fragment where all variables have been substituted
;; for data. Note that a RuleFragment is "open by default", while in contrast
;; a Fact is "closed by default", hence the inconsistent naming convention

(struct rule-frag [name terms choices is??] #:transparent
 #:methods gen:equal+hash
  [(define (equal-proc self other rec)
     ;; this first check is a hack, so that symbol comparison works in tests...
     ;; TODO: fix this by implementing a more proper equality check
     (and (or (equal? (rule-frag-name self) (rule-frag-name other))
              (bound-identifier=? (rule-frag-name self)
                                  (rule-frag-name other)))
          (rec (rule-frag-terms self) (rule-frag-terms other))
          (rec (rule-frag-choices self) (rule-frag-choices other))
          (rec (rule-frag-is?? self) (rule-frag-is?? other))))
   (define (hash-proc self rec)
     (hash-code-combine (eq-hash-code struct:rule-frag)
                        (rec (rule-frag-terms self))
                        (rec (rule-frag-choices self))
                        (rec (rule-frag-is?? self))))
   (define (hash2-proc self rec)
     (hash-code-combine (eq-hash-code struct:rule-frag)
                        (rec (rule-frag-terms self))
                        (rec (rule-frag-choices self))
                        (rec (rule-frag-is?? self))))])

;; A Rule is a (rule RuleFragment [ListOf OpenFact])
;; It represents a conclusion which follows from the (0 or more) premises
;; All occurrences of variables in the conclusion must be bound by
;; the premises. A conclusion with no premises is just a fact.

(struct rule [conclusion premises] #:transparent)

;; A Program is a (logic [ListOf Rule] [ListOf Rule])
;; storing the rules which never require making a choice in the conclusion
;; and the rules which require making choices (> 1 option on RHS of `is`)

(struct program [deduce-rules choose-rules] #:transparent)

;; A Solution is an opaque object that can be queried for propositions.

;; For now (naively), a Solution is a (solution Database)
;; (where Database is documented in "database.rkt", which we require)

(struct solution [database] #:transparent)
