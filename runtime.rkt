#lang racket

;; these are not provided as `struct-out` since we only
;; have to construct from the compiler, nothing else
(provide (rename-out [make-fact fact])
         variable
         rule-frag
         rule
         logic
         all)

(require racket/stream "utils.rkt")

;; A [Maybe X] is one of:
;; - #f
;; - X
;; and represents a value that might not exist.

;; NONE represents the absence of a value
(define NONE (gensym))

;; none? : Any -> Bool
(define (none? x)
  (equal? NONE x))

;; A [Option X] is one of:
;; - NONE
;; - X
;; and represents a value that might not exist, using a sentinel value
;; to differentiate between the NONE case and #f being present

;; A Datum is RacketDatum
;; Ideally, we would allow any Racket datum here, but this may change
;; if implementing this is infeasible.
;; CURRENTLY, it seems as if we will need to be able to compare for equality
;; to compare attributes and determine when conflicts arise

;; A Fact is a (fact Symbol [ListOf Datum] [Option Datum]).
;; It represents a known fact (either given or deduced) in the database.

;; name + terms = "attribute"
;; if we need to use attributes without values, we can abstract
(struct fact [name terms value] #:transparent)

(define (make-fact name terms [value NONE])
  (fact name terms value))

;; A Database is a [ListOf Fact]
;; TODO: make this something with more structure for faster lookup

;; A Variable is a (variable Symbol)
(struct variable [name] #:transparent)

;; A Term is one of:
;; - Datum
;; - Variable

;; An OpenFact is a (fact Symbol [ListOf Term] [Option Term])
;; It represents a fact that has not yet been fully grounded.

;; A RuleFragment is (rule-frag Symbol [ListOf Term] [ListOf Term]).
;; It is an _internal representation_ of the source syntax, which in
;; general may contain variables. These can be combined to form rules
;; or facts (closed rules with no premises).
;; Notably, a Term is an immediate, and does not have nested terms:
;; this is a difference from the surface <term> syntax; these nested
;; terms will be desugared in an ANF-like pass.

;; A ClosedRuleFragment is (rule-frag Symbol [ListOf Datum] [ListOf Datum]).
;; it represents a rule fragment where all variables have been substituted
;; for data. Note that a RuleFragment is "open by default", while in contrast
;; a Fact is "closed by default", hence the inconsistent naming convention

(struct rule-frag [name terms choices] #:transparent)

;; A Rule is a (rule RuleFragment [ListOf OpenFact])
;; It represents a conclusion which follows from the (0 or more) premises
;; All occurrences of variables in the conclusion must be bound by
;; the premises. A conclusion with no premises is just a fact.

(struct rule [conclusion premises] #:transparent)

;; define-logic binds the identifier to a `Logic`
;; `Logic` is an opaque object that can be used to obtain solutions.

;; Internally, we know that a Logic is a (logic [ListOf Rule] [ListOf Rule])
;; storing the rules which never require making a choice in the conclusion
;; and the rules which require making choices (> 1 option on RHS of `is`)
;; TODO: add in information about imported Racket function(al relation)s

(struct logic [deduce-rules choose-rules] #:transparent)

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
;; (a now-grounded conclusion), and indices of choices which have been made.

(struct search-state [database conclusion tried])

;; A SearchStack is a [ListOf SearchState]

;; (deduce) Database Logic -> [Maybe Database]
;; pick a rule that does not require choosing, and try doing
;; Database Rule -> [Maybe Database]
;; (choose) Database Logic SearchStack -> Database SearchStack

;; A SolutionResult is [Maybe [PairOf Solution SearchStack]]

;; An ConsistencyResult is one of:
;; - 'inconsistent
;; - #f
;; - [PairOf Solution SearchStack]
;; and represents a SolutionResult where an inconsistency may have been reached

;; A Substitution is a [HashOf Symbol Datum]
;; and represents a substitituon for variables in an OpenFact.

;; sample : Logic -> [Maybe Solution]
;; Obtain one possible solution of the given program, if one exists.

;; all : Logic -> [StreamOf Solution]
;; Obtain a stream of all possible solutions of the given program.
;; The stream may be infinite, and computing the next item may not
;; always terminate.
(define (all prog)
  ;; result->stream : SolutionResult -> [StreamOf Solution]
  ;; Processes a SolutionResult into a stream of Solution by recursively
  ;; backtracking through all possible intermediate choices.
  (define (result->stream x)
    (match x
      [#f empty-stream]
      [(cons sol next-st) (stream-cons sol (collect-backtracked next-st))]))
  ;; collect-backtracked : SearchStack -> [StreamOf Solution]
  (define (collect-backtracked stack)
    (result->stream (backtrack prog stack)))
  
  (result->stream (solve prog '() '())))

;; solve : Logic Database SearchStack -> SolutionResult
;; Given a search state and a database of currently known facts, obtain a
;; single solution to the program, while tracking choices made. 
(define (solve prog db stack)
  ; if there is an inconsistency reached, we backtrack in an attempt
  ; to get to a better spot in our tree. if all things are inconsistencies,
  ; then backtrack and solve will call each other and eventually return #f

  ;; TODO: clean up. this is clear, but ugly...
  ;; also, all calls are in tail position, to prevent stack overflow
  (match (deduce (logic-deduce-rules prog) db stack)
    [(cons new-db new-st) (solve prog new-db new-st)]
    ['inconsistent ; triggers a backtrack
     (backtrack prog stack)]
    [#f
     ;; if we could not deduce, we instead try to choose
     (match (choose (logic-choose-rules prog) db stack)
       [(cons new-db new-st) (solve prog new-db new-st)]
       ['inconsistent ; triggers a backtrack
        (backtrack prog stack)]
       [#f
        ;; if no new deductions or choices are left to be made, we are
        ;; saturated and have reached a solution!
        (cons db stack)])]))

;; backtrack : Logic SearchStack -> SolutionResult
;; Backtrack from the given search state and find a new solution to the given
;; program.
(define (backtrack prog stack)
  ; to backtrack:
  ; - look at the search state on the top of the stack
  ; - if there are choices which have not been made, make one of those and go
  ; - otherwise, pop off of the stack and try again, a level down

  ;; update-stack : SearchStack Nat -> SearchStack
  ;; update the top of the search stack with the new choice made.
  (define (update-stack stack idx)
    (define state (first stack))
    (cons (search-state (search-state-database state)
                        (search-state-conclusion state)
                        (set-add (search-state-tried state) idx))
          (rest stack)))

  ;; add-to-db : SearchState Nat -> Database
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
                     (add-to-db current-state choice-idx)
                     (update-stack stack choice-idx)))))))

;; fire-first-rule : [Rule Database SearchStack -> ConsistencyResult]
;;                   [ListOf Rule]
;;                   Database
;;                   SearchStack
;;                   -> ConsistencyResult
;; abstracts the differences between deduction and choice
;; finds the first rule which fires and fires it, using the supplied
;; function. Returns #f if nothing fires and 'inconsistent if inconsistency
(define (fire-first-rule fire-rule rules db stack)
  (match rules
    ['() #f]
    [(cons rule rules)
     (match (fire-rule rule db stack)
       [#f (fire-first-rule fire-rule rules db stack)]
       ;; propogates inconsistency, or returns the new good db + stack
       [res res])]))

;; deduce-with-rule : Rule Database SearchStack -> ConsistencyResult
;; Attempts to use the given rule to make a new deduction
;; If no deductions can be made, #f; if inconsistent, 'inconsistent
(define (deduce-with-rule rule db stack)
  ;; is-good-subst? : Substitution -> Bool
  ;; in this context, a substitution is "good" if it lets us deduce a new fact
  (define (is-good-subst? subst)
    (not (member (rule-frag->fact (ground (rule-conclusion rule) subst)) db)))

  (match (inst-premises (rule-premises rule) (hash) db is-good-subst?)
    [#f #f]
    [subst
     (let ([new-fact (rule-frag->fact (ground (rule-conclusion rule) subst))])
       (if (consistent? db new-fact)
           (cons (cons new-fact db) stack)
           'inconsistent))]))

;; choose-with-rule : Rule Database SearchStack -> ConsistencyResult
;; Attempts to use the given rule to make a new deduction
;; If no deductions can be made, #f; if inconsistent, 'inconsistent
(define (choose-with-rule rule db stack)
  (raise 'implement-me)
  ;; is-good-subst? : Substitution -> Bool
  ;; in this context, a substitution is "good" if it lets us deduce a new fact
  #;(define (is-good-subst? subst)
    (not (member (rule-frag->fact (ground (rule-conclusion rule) subst)) db)))

  #;(match (inst-premises (rule-premises rule) (hash) db is-good-subst?)
    [#f #f]
    [subst
     (let ([new-fact (rule-frag->fact (ground (rule-conclusion rule) subst))])
       (if (consistent? db new-fact)
           (cons (cons new-fact db) stack)
           'inconsistent))]))

;; deduce : [ListOf Rule] Database SearchStack -> ConsistencyResult
;; Makes one deduction if possible, using the first deduction rule which fires.
(define deduce ((curry fire-first-rule) deduce-with-rule))

;; choose : [ListOf Rule] Database SearchStack -> ConsistencyResult
;; Makes one choice if possible, using the first deduction rule which fires.
(define choose ((curry fire-first-rule) choose-with-rule))

;; basic algorithm:
;; - get everything in db that looks like current(p)
;;   - same symbol, same length
;;   - when we get '(x), that's a known true fact that we can use
;;   - if we get '(), the current substitution will not work, backtrack
;;     - go back to when the last assignment choice was made, and try
;;       again
;; - for each, try to make those choices of variable assignments
;;   - add to current the remaining variables, map to whatever the known
;;     fact says
;;   - recur down the list for the other premises, using the new subst

;; inst-premises : [ListOf OpenFact]
;;                 Substitution
;;                 Database
;;                 [Substitution -> Bool]
;;                 -> [Maybe Substitution]
;; instantiates the premises using the known facts and accumulated substitution
;; to produce a new substitution which satisfies the given predicate
(define (inst-premises prems current-subst db is-good-subst?)
  ;; TODO: optimize by checking already known facts about conclusion
  (match prems
    ['()
     ; if the substitution is good then we return it, otherwise we give #f
     ; which will "backtrack" AKA try again with remaining candidates
     (and (is-good-subst? current-subst) current-subst)]
    [(cons p prems)
     (define (try-ground candidates)
       (match candidates
         ; we have no more facts that work, so this substitution
         ; is wrong, we backtrack
         ['() #f]
         [(cons f facts)
          ; we try to instantiate the first fact as variable
          ; assignments, and recur until something (or nothing) works
          (define new-subst (update-subst current-subst p f))
          (match (inst-premises prems new-subst db is-good-subst?)
            [#f (try-ground facts)]
            [the-good-subst the-good-subst])]))
     (try-ground (find-facts db p current-subst))]))

;; find-facts : Database OpenFact Substitution -> [ListOf Fact]
;; Get a list of facts from the database that "look like" the given open fact.
;; A fact "looks like" an open fact if it has the same relation symbol, and
;; all non-variable positions have equal values.
(define (find-facts db open subst)
  ;; similar? : Term Datum -> Boolean
  (define (similar? term datum)
    (match term
      [(variable n) (if (hash-has-key? subst n)
                        (equal? (hash-ref subst n) datum)
                        #t)]
      [d (equal? d datum)]))
  
  ;; looks-like? : Fact -> Boolean
  (define (looks-like? fact)
    (and (symbol=? (fact-name open) (fact-name fact))
         (andmap similar?
                 (fact-terms open)
                 (fact-terms fact))
         (or (and (none? (fact-value open))
                  (none? (fact-value fact)))
             (similar? (fact-value open) (fact-value fact)))))
  
  (filter looks-like? db))

;; update-subst : Substitution OpenFact Fact -> Substitution
;; PRECONDITION: f "looks like" open, as defined by `looks-like?`.
(define (update-subst sub open f)
  ;; assign-var : Term Datum Substitition -> Substitution
  ;; if t is a variable that does not occur in curr-sub, returns curr-sub[d/t]
  (define (assign-var t d curr-sub)
    (match t
      [(variable n) (if (hash-has-key? curr-sub n)
                        curr-sub
                        (hash-set curr-sub n d))]
      [_ curr-sub]))
  
  (assign-var (fact-value open)
              (fact-value f)
              (foldl assign-var
                     sub
                     (fact-terms open)
                     (fact-terms f))))

;; consistent? : Database Fact -> Bool
;; Determine if the given fact is consistent with the database of already
;; known facts.
(define (consistent? db f)
  ;; fact-consistent? : Fact -> Bool
  ;; Determine if the known fact is consistent with the closed over fact f.
  ;; Two facts are consistent if they do not map the same attribute to
  ;; different values.
  (define (fact-consistent? known)
    (not (and (equal? (fact-name f) (fact-name known))
              (equal? (fact-terms f) (fact-terms known))
              (not (equal? (fact-value f) (fact-value known))))))
  
  (andmap fact-consistent? db))

;; ground : RuleFragment Substitution -> ClosedRuleFragment
;; PRECONDITION: subst must contain assignments for all variables that appear
;; which will always be the case, due to static checks in the parser/compiler.
;; Closes off the (possibly open) RuleFragment using the given substitution
(define (ground open subst)
  ;; ground-term : Term -> Datum
  (define (ground-term t)
    (match t
      [(variable name) (hash-ref subst name)]
      [_ t]))
  
  (rule-frag (rule-frag-name open)
             (map ground-term (rule-frag-terms open))
             (map ground-term (rule-frag-choices open))))

;; rule-frag->fact : ClosedRuleFragment -> Fact
;; PRECONDITION: the ClosedRuleFragment must be making 0 or 1 choices
(define (rule-frag->fact frag)
  (define fact-value
    (match (rule-frag-choices frag)
      ['() NONE]
      [(list c) c]
      [_ (error 'rule-frag->fact "rule fragment has multiple choices")]))
  (fact (rule-frag-name frag) (rule-frag-terms frag) fact-value))

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

(module+ test
  (require rackunit)
  
  (check-equal?
   (stream->list (all (logic
                       (list (rule (rule-frag 'foo '(1) '(a)) '()))
                       '())
                      ))
   (list (list (fact 'foo '(1) 'a)))))