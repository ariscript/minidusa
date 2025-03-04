#lang racket

(require racket/stream "utils.rkt")

;; A [Maybe X] is one of:
;; - #f
;; - X
;; and represents a value that might not exist.

;; A (none) represents the absence of a value
(struct none [])

;; A [Option X] is one of:
;; - (none)
;; - X
;; and represents a value that might not exist, using a sentinel value
;; to differentiate between the (none) case and #f being present

;; A Datum is RacketDatum
;; Ideally, we would allow any Racket datum here, but this may change
;; if implementing this is infeasible.
;; TODO : maybe revisit this

;; A Fact is a (fact Symbol [ListOf Datum] [Option Datum]).
;; It represents a known fact (either given or deduced) in the database.

;; name + terms = "attribute"
;; if we need to use attributes without values, we can abstract
(struct fact [name terms value]
  ;; do we want this?
  #:transparent)

(define (make-fact name terms [value (none)])
  (fact name terms value))

;; A Database is a [ListOf Fact]

;; A Variable is a (variable Symbol)
(struct variable [name])

;; A Term is one of:
;; - Datum
;; - Variable

;; A RuleFragment is (rule-frag Symbol [ListOf Term] [ListOf Term]).
;; It is an internal representation of the source syntax, which in
;; general may contain variables. These can be combined to form rules
;; or facts (closed rules with no premises)

(struct rule-frag [name terms choices])

;; A Rule is a (rule RuleFragment [ListOf RuleFragment])
;; It represents a conclusion which follows from the (0 or more) premises
;; All occurrences of variables in the conclusion must be bound by
;; the premises. A conclusion with no premises is just a fact.

(struct rule [conclusion premises])


;; define-logic binds the identifier to a `Logic`
;; `Logic` is an opaque object that can be used to obtain solutions.

;; Internally, we know that a Logic is a (logic [ListOf Rule])
;; TODO: add in information about imported Racket function(al relation)s

(struct logic [rules])

;; A Solution is an opaque object that can be queried for propositions.

;; For now (naively), a Solution is a (solution Database)

(struct solution [database])


;; we have some database (D) of known facts
;; we want to evolve this database
;; 1. Pick a rule to try to apply
;;    - Deduce, then choose
;; 2. See if its premises can be satisfied by D
;;    - This will involve picking a substitution to ground
;; 3. If so, add the conclusion
;;    - Or, make a choice of conclusions if necessary
;; Repeat until all rules cannot fire any more

;; construct some tree-like structure where nodes are present
;; at choice points, stashing away enough information to resume
;; when backtracking (ex: database at that point in time)

;; we want a stack of choice points
;; choice points in the stack have to contain enough information
;; to ensure that we don't make the same choice multiple times

;; A SearchState is a (search-state Database RuleFragment [SetOf Nat]).
;; It represents information associated with a choice point: namely, the facts
;; known to be true at the time of choice, the options to choose from
;; (a now-grounded conclusion), and the indices of choices which have been made.

(struct search-state [database conclusion tried])

;; A SearchStack is a [ListOf SearchState]

;; (deduce) Database Logic -> [Maybe Database]
;; pick a rule that does not require choosing, and try doing
;; Database Rule -> [Maybe Database]
;; (choose) Database Logic SearchStack -> Database SearchStack

;; A SolutionResult is [Maybe [PairOf Solution SearchStack]]

;; sample: Logic -> [Maybe Solution]
;; Obtain one possible solution of the given program, if one exists.

;; all: Logic -> [StreamOf Solution]
;; Obtain a stream of all possible solutions of the given program.
;; The stream may be infinite, and computing the next item may not
;; always terminate.

(define (all prog)
  ;; SolutionResult -> [StreamOf Solution]
  ;; Processes a SolutionResult into a stream of Solution by recursively
  ;; backtracking through all possible intermediate choices.
  (define (result->stream x)
    (match x
      [#f (empty-stream)]
      [(cons sol next-st) (stream-cons sol (collect-backtracked next-st))]))
  
  ; SearchStack -> [StreamOf Solution]
  (define (collect-backtracked stack)
    (result->stream (backtrack prog stack)))
  (result->stream (solve prog '() '())))

;; solve: Logic SearchStack Database -> SolutionResult
;; Given a search state and a database of currently known facts, obtain a
;; single solution to the program, while tracking choices made.

(define (solve prog stack db)
  #f)

;; backtrack: Logic SearchStack -> SolutionResult
;; Backtrack from the given search state and find a new solution to the given
;; program.
(define (backtrack prog stack)
  ; to backtrack:
  ; - look at the search state on the top of the stack
  ; - if there are choices which have not been made, make one of those and go
  ; - otherwise, pop off of the stack and try again, a level down

  ;; SearchStack Nat -> SearchStack
  ;; update the top of the search stack with the new choice made.
  (define (update-stack stack idx)
    (define state (first stack))
    (cons (search-state (search-state-database state)
                        (search-state-conclusion state)
                        (set-add (search-state-tried state) idx))
          (rest stack)))

  ;; SearchState Nat -> Database
  ;; Update the given database with the newly chosen fact. The conclusion
  ;; rule fragement is closed. We make the `idx`-th choice out of all possible
  ;; conclusion choices.
  (define (add-to-db state idx)
    (define db (search-state-database state))
    (define conc (search-state-conclusion state))
    (cons (make-fact (rule-frag-name conc)
                     (rule-frag-terms conc)
                     (list-ref (rule-frag-choices conc) idx)) db))
  
  (if (empty? stack) #f
      (let* ([current-state (first stack)]
             [all-choices (rule-frag-choices
                           (search-state-conclusion current-state))]
             [indices-tried (search-state-tried current-state)]
             [untried (filteri (lambda (_ i)
                                 (not (set-member? indices-tried i))))])
        (if (empty? untried) (backtrack prog (rest stack))
            (let ([choice-idx (random (length untried))])
              (solve prog
                     (update-stack stack choice-idx)
                     (add-to-db current-state choice-idx)))))))

;; has: Solution Symbol Datum ... -> Bool
;; Returns `#t` if the given proposition exists in this solution.
;; Raises an error if the number of arguments provided does not match the
;; number of arguments defined in the original relation.

;; get: Solution Symbol Datum ... -> Datum
;; Returns the value associated with the given proposition in this
;; solution.
;; Raises an error if the number of arguments provided does not match the
;; number of arguments defined in the original relation.

;; lookup: Solution Symbol Datum ... -> [ListOf [ListOf Datum]]
;; Query the solution for a proposition of the form provided. Providing
;; fewer arguments than the original definition results in the rest being
;; treated as wildcards, and included in the inner lists.
;; If provided with _more_ arguments than the original definition, `lookup`
;; raises an error.

;; facts: Solution -> [ListOf Fact]
;; Return a list of all known facts in this solution.