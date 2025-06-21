#lang racket

;; these are not provided as `struct-out` since we only
;; have to construct from the compiler, nothing else
(provide all
         has
         get)

(require racket/stream
         "data.rkt"
         "database.rkt")

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

(struct search-state [database conclusion tried] #:transparent)

;; A SearchStack is a [ListOf SearchState]

;; (deduce) Database Logic -> [Maybe Database]
;; pick a rule that does not require choosing, and try doing
;; Database Rule -> [Maybe Database]
;; (choose) Database Logic SearchStack -> Database SearchStack

;; A DatabaseState is a (db-state Database SearchStack)
(struct db-state [db stack] #:transparent)

;; A SolutionResult is [Maybe DatabaseState]

;; An ConsistencyResult is one of:
;; - (inconsistent SearchStack)
;; - #f
;; - DatabaseState
;; and represents a SolutionResult where an inconsistency may have been reached
(struct inconsistent [stack] #:transparent)

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
      [(db-state final-db next-st)
       (stream-cons (solution final-db) (collect-backtracked next-st))]))
  ;; collect-backtracked : SearchStack -> [StreamOf Solution]
  (define (collect-backtracked stack)
    (result->stream (backtrack prog stack)))

  (result->stream (solve prog (db-of) '())))

;; solve : Logic Database SearchStack -> SolutionResult
;; Given a search state and a database of currently known facts, obtain a
;; single solution to the program, while tracking choices made.
(define (solve prog db stack)
  ; if there is an inconsistency reached, we backtrack in an attempt
  ; to get to a better spot in our tree. if all things are inconsistencies,
  ; then backtrack and solve will call each other and eventually return #f

  ; TODO: clean up. this is clear, but ugly...
  ; also, all calls are in tail position, to prevent stack overflow
  (match (deduce (program-deduce-rules prog) db stack)
    [(db-state new-db new-st) (solve prog new-db new-st)]
    [(inconsistent stack) (backtrack prog stack)]
    [#f
     ; if we could not deduce, we instead try to choose
     (match (choose (program-choose-rules prog) db stack)
       [(db-state new-db new-st) (solve prog new-db new-st)]
       [(inconsistent stack) (backtrack prog stack)]
       [#f
        ; valid solutions must have positive databases
        ; otherwise, we treat it as inconsistent
        (if (db-positive? db)
            (db-state db stack)
            (backtrack prog stack))])]))

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

  ;; update-stack/constraint : SearchStack -> SearchStack
  ;; update the top of the stack to track that we added a constraint.
  (define (update-stack/constraint stack)
    (define state (first stack))
    (cons (search-state (search-state-database state)
                        (struct-copy rule-frag (search-state-conclusion state)
                                     [is?? 'tried])
                        (search-state-tried state))
          (rest stack)))

  (if (empty? stack) #f
      (let* ([current-state (first stack)]
             [current-db (search-state-database current-state)]
             [conclusion (search-state-conclusion current-state)]
             [all-choices (rule-frag-choices conclusion)]
             [indices-tried (search-state-tried current-state)]
             ; for now, we choose the next choice in order, for easier testing
             ; we still use a set to enable future changes to the order
             [idx-to-try (add1 (apply max (set->list indices-tried)))])

        (cond [(< idx-to-try (length all-choices))
               (define new-fact (rule-frag->fact/choose conclusion idx-to-try))
               (define new-stack (update-stack stack idx-to-try))
               (if (consistent? current-db new-fact)
                   (solve prog (db-cons new-fact current-db) new-stack)
                   ; backtrack, but make the next choice
                   (backtrack prog new-stack))]
              [(and (= idx-to-try (length all-choices))
                    (equal? #t (rule-frag-is?? conclusion)))
               (define new-constraint (rule-frag->constraint conclusion))
               (define new-stack (update-stack/constraint stack))
               (if (consistent? current-db new-constraint)
                   (solve prog
                          (db-add-constraint new-constraint current-db)
                          new-stack)
                   ; backtrack, but make the next choice
                   (backtrack prog new-stack))]
              [else
               ; if we ran out of choices, then pop off the stack
               (backtrack prog (rest stack))]))))

;; pick-and-fire-rule : [Rule Database SearchStack -> ConsistencyResult]
;;                      [ListOf Rule]
;;                      Database
;;                      SearchStack
;;                      -> ConsistencyResult
;; abstracts the differences between deduction and choice
;; finds the first rule which fires and fires it, using the supplied
;; function. Returns #f if nothing fires and 'inconsistent if inconsistency
(define (pick-and-fire-rule fire-rule rules db stack)
  (match rules
    ['() #f]
    [(cons rule rules)
     (match (fire-rule rule db stack)
       [#f (pick-and-fire-rule fire-rule rules db stack)]
       ; propogates inconsistency, or returns the new good db + stack
       [res res])]))

;; deduce-with-rule : Rule Database SearchStack -> ConsistencyResult
;; Attempts to use the given rule to make a new deduction
;; If no deductions can be made, #f; if inconsistent, return inconsistent
;; with the same stack
(define (deduce-with-rule rule db stack)
  ;; is-good-subst? : Substitution -> Bool
  ;; in this context, a substitution is "good" if it lets us deduce a new fact
  (define (is-good-subst? subst)
    (not (db-has? (rule-frag->fact (ground subst (rule-conclusion rule))) db)))

  (match (inst-premises (rule-premises rule) (hash) db is-good-subst?)
    [#f #f]
    [subst
     (define new-fact (rule-frag->fact (ground subst (rule-conclusion rule))))
     (if (consistent? db new-fact)
         (db-state (db-cons new-fact db) stack)
         (inconsistent stack))]))

;; choose-with-rule : Rule Database SearchStack -> ConsistencyResult
;; Attempts to use the given rule to make a new deduction
;; If no deductions can be made, #f; if inconsistent, return inconsistent
;; with the new stack that was tried
(define (choose-with-rule rule db stack)
  ;; is-good-subst? : Substitution -> Bool
  ;; in this context, a substitution is "good" if the conclusion grounded
  ;; by the substitution is different from all previous choices made
  ;; (good substitutions will let us make new choices or find inconsistencies)
  (define (is-good-subst? subst)
    ; TODO: optimize by filtering out inconsistent choices
    (define grounded-conclusion (ground subst (rule-conclusion rule)))

    ; 'tried = #t for our purposes
    (define (conclusion=? a b)
      (and (equal? (rule-frag-name a) (rule-frag-name b))
           (equal? (rule-frag-terms a) (rule-frag-terms b))
           (equal? (rule-frag-choices a) (rule-frag-choices b))
           (equal? (not (rule-frag-is?? a)) (not (rule-frag-is?? b)))))
    
    (andmap (lambda (state)
              (not (conclusion=? (search-state-conclusion state)
                                 grounded-conclusion)))
            stack))

  (match (inst-premises (rule-premises rule) (hash) db is-good-subst?)
    [#f #f]
    [subst
     (define new-conc (ground subst (rule-conclusion rule)))
     ; we initially pick the 0-th choice
     (define new-state (search-state db new-conc (set 0)))
     (define new-fact (rule-frag->fact/choose new-conc 0))
     (if (consistent? db new-fact)
         (db-state (db-cons new-fact db) (cons new-state stack))
         (inconsistent (cons new-state stack)))]))

;; deduce : [ListOf Rule] Database SearchStack -> ConsistencyResult
;; Makes one deduction if possible, using the first deduction rule which fires.
(define deduce ((curry pick-and-fire-rule) deduce-with-rule))

;; choose : [ListOf Rule] Database SearchStack -> ConsistencyResult
;; Makes one choice if possible, using the first deduction rule which fires.
(define choose ((curry pick-and-fire-rule) choose-with-rule))

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
     ;; try-ground : Database -> [Maybe Substitution]
     (define (try-ground candidates)
       (if (db-empty? candidates) #f
           (let ([new-subst
                  (update-subst current-subst p (db-first candidates))])
             (match (inst-premises prems new-subst db is-good-subst?)
               [#f (try-ground (db-rest candidates))]
               [the-good-subst the-good-subst]))))
     (try-ground (find-facts db p current-subst))]))

;; find-facts : Database OpenFact Substitution -> Database
;; Get a list of facts from the database that "look like" the given open fact.
;; This is like a "sub-database". We think of the database as containing facts
;; associating inputs to procedure outputs for imported procedures
;;
;; A fact `f` "looks like" an open fact `open` if there exists an extension
;; `subst'` to the given substitution `subst` such that `f` is `subst'(open)`,
;; which represents grounding the open fact using subst'
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
    (and (symbol=? (fact-rel open) (fact-rel fact))
         (andmap similar?
                 (fact-terms open)
                 (fact-terms fact))
         (or (and (none? (fact-value open))
                  (none? (fact-value fact)))
             (similar? (fact-value open) (fact-value fact)))))

  (if (symbol? (fact-rel open))
      (db-filter looks-like? db)
      ; otherwise, we have a proc, which we run on the result of closing terms
      ; (which will always be fully groundable, by our static checks)
      (let* ([substituted (apply-subst subst open)]
             [proc-result (apply (fact-rel substituted)
                                 (fact-terms substituted))]
             [the-singleton-db
              (db-cons (fact (fact-rel substituted)
                             (fact-terms substituted)
                             proc-result)
                       (db-of))]
             [var-or-known-val (fact-value substituted)])
        ; the proc-result should agree with the value, if it is known
        ; always "looks like" if we have a variable in that position
        (if (or (variable? var-or-known-val)
                (equal? var-or-known-val proc-result))
            the-singleton-db
            (db-of)))))

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

;; make-term-grounder : Subst -> [Term -> Term]
;; Returns a function which substitutes using subst, which will bring
;; variables to whatever subst maps them to, if it has a binding for it
(define (make-term-grounder subst)
  (lambda (t)
    (match t
      [(variable name)
       #:when (hash-has-key? subst name)
       (hash-ref subst name)]
      [_ t])))

;; apply-subst : Subst OpenFact -> OpenFact
;; Substitutes as many of the variables in open as possible.
;; If the substitution contains all of the free variables of OpenFact,
;; then the result will be a Fact
(define (apply-subst subst open)
  (define ground-term (make-term-grounder subst))
  (fact (fact-rel open)
        (map ground-term (fact-terms open))
        (ground-term (fact-value open))))

;; ground : Substitution RuleFragment -> ClosedRuleFragment
;; PRECONDITION: subst must contain assignments for all variables that appear
;; which will always be the case, due to static checks in the parser/compiler.
;; Closes off the (possibly open) RuleFragment using the given substitution
(define (ground subst open)
  (define ground-term (make-term-grounder subst))
  (rule-frag (rule-frag-name open)
             (map ground-term (rule-frag-terms open))
             (map ground-term (rule-frag-choices open))
             (rule-frag-is?? open)))

;; rule-frag->fact : ClosedRuleFragment -> Fact
;; PRECONDITION: the ClosedRuleFragment must be making 0 or 1 choices
(define (rule-frag->fact frag)
  (define fact-value
    (match (rule-frag-choices frag)
      ['() NONE]
      [(list c) c]
      [_ (error 'rule-frag->fact "rule fragment has multiple choices")]))
  (fact (rule-frag-name frag) (rule-frag-terms frag) fact-value))

;; rule-frag->fact/choose : ClosedRuleFragment Nat -> Fact
;; Turns a closed rule fragment into a fact by making the `idx`-th choice
(define (rule-frag->fact/choose frag idx)
  (make-fact (rule-frag-name frag)
             (rule-frag-terms frag)
             (list-ref (rule-frag-choices frag) idx)))

;; rule-frag->constraint : ClosedRuleFragment -> Constraint
;; Turns a closed rule fragment into a constraint (saying that none of the
;; choices are allowed).
(define (rule-frag->constraint frag)
  (constraint (rule-frag-name frag)
              (rule-frag-terms frag)
              (rule-frag-choices frag)))

;; has: Solution Symbol Datum ... -> Bool
;; Returns `#t` if the given relation on the provided terms exists in
;; this solution, either as a functional relation or a normal relation
(define (has sol rel . terms)
  (define (same-attr f)
    (and (equal? (fact-rel f) rel)
         (equal? (fact-terms f) terms)))
  ;; TODO: maybe raise an error for unexpected arguments / etc?
  (not (db-empty? (db-filter same-attr (solution-database sol)))))

;; get: Solution Symbol Datum ... -> Datum
;; Returns the value associated with the given attribute in this
;; solution, if one exists. Raises an error if the attribute is not
;; in the solution; returns NONE if the attribute is not a functional relation
(define (get sol rel . terms)
  (define (same-attr f)
    (and (equal? (fact-rel f) rel)
         (equal? (fact-terms f) terms)))
  ;; TODO: maybe raise an error for unexpected arguments / etc?
  ;; TODO: this raises a bad error currently when the fact is not found
  (fact-value (db-first (db-filter same-attr (solution-database sol)))))

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

  (define simple-program
    (program
     (list (rule (rule-frag 'foo '(1) '(a) #f) '()))
     '()))
  
  (check-equal?
   (stream->list (all simple-program))
   (list (solution (db-of (fact 'foo '(1) 'a)))))

  (check-equal?
   (has (stream-first (all simple-program)) 'foo 1)
   #t)
  
  (check-equal?
   (has (stream-first (all simple-program)) 'foo 2)
   #f)

  (check-equal?
   (get (stream-first (all simple-program)) 'foo 1)
   'a)

  (check-exn
   ;; TODO: make this error message better
   #rx""
   (lambda () (get (stream-first (all simple-program)) 'foo 2)))
  
  ;; we can run imported relations
  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() '() #f)
                                   (list (fact add1 '(0) 1))))
                       '())))
   (list (solution (db-of (make-fact 'foo '())))))

  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() '() #f)
                                   (list (fact * '(0 1 2) 1))))
                       '())))
   (list (solution (db-of))))

  ;; example where we bind a variable based on an import
  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() (list (variable 'X)) #f)
                                   (list (fact + '(1 2 3 4) (variable 'X)))))
                       '())))
   (list (solution (db-of (make-fact 'foo '() 10))))))

(define prog
  (program (list (rule (rule-frag 'ok '() '(#t) #f) '())
                 (rule (rule-frag 'ok '() '(#f) #f) (list (fact 'a '() 1))))
           (list (rule (rule-frag 'a '() '(1) #t) '()))))
(define deduced (deduce (program-deduce-rules prog) (db-of) '()))
(define deduced* (deduce (program-deduce-rules prog) (db-state-db deduced) '()))
(define chosen (choose (program-choose-rules prog) (db-state-db deduced) '()))
(define deduced** (deduce (program-deduce-rules prog) (db-state-db chosen)
                          (db-state-stack chosen)))
(define backtracked (backtrack prog (inconsistent-stack deduced**)))
(define new-db (db-add-constraint
                                (rule-frag->constraint
                                 (rule-frag 'a '() '(1) 'tried))
                                (db-of (fact 'ok '() #t))))
#;(define add-constraint (deduce (program-deduce-rules prog)
                               new-db
                               (db-state-stack backtracked)))
