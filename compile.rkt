#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example

(require (for-template
          racket/base
          syntax-spec-v3
          (prefix-in rt: "runtime.rkt"))
         syntax/parse)

;; EXAMPLE EXPANSIONS!
#;(logic
   (foo 1))
;; =>
#;(rt:logic (list (rt:rule (rt:rule-frag 'foo '(1) '())
                           '()))
            '())

#;(logic
   ((foo 2) :- (foo 1))
   (foo 1))
;; =>
#;(rt:logic (list (rt:rule (rt:rule-frag 'foo '(2) '())
                           (list (rt:fact 'foo '(1))))
                  (rt:rule (rt:rule-frag 'foo '(1) '())
                           '()))
            '())

#;(logic
   (foo 1)
   (is (bar 10 11) (choice 1 2 3)))
;; =>
#;(rt:logic (list (rt:rule (rt:rule-frag 'foo '(1) '())
                           '()))
            (list (rt:rule (rt:rule-frag 'bar '(10 11) '(1 2 3))
                           '())))

#;(logic
   ((foo X) :- (is (bar) X) (baz)))
;; =>
#;(rt:logic (list (rt:rule (rt:rule-frag 'foo (list (rt:variable 'X)) '())
                           (list (rt:fact 'bar '() (rt:variable 'X))
                                 (rt:fact 'baz '()))))
            '())

;; this is the old compile-time function
;; ImportsSyntax LogicSyntax -> RacketSyntax
(define (compile-logic imports-stx logic-stx)
  (define rel-arities (make-hash))
  (define imported-rel-vars (mutable-set))
  ; add all imported symbols; later we use this to compile attibutes
  (for ([stx-pair (syntax->list imports-stx)])
    (syntax-parse stx-pair
      [[rel-id _]
       ; we want to put the symbols in our symbol table, not the identifiers,
       ; otherwise we will not have the right notion of element equality
       (set-add! imported-rel-vars (syntax->datum #'rel-id))]))

  (define body
    (let ([compile-decl ((curry compile-decl) rel-arities imported-rel-vars)])
      (syntax-parse logic-stx
        [(d ...+)
         ;; like define/syntax-parse, but shorter
         #:with (deduce-rule ...)
         (map compile-decl (filter (negate is-choose?) (attribute d)))

         #:with (choose-rule ...)
         (map compile-decl (filter is-choose? (attribute d)))
     
         #'(rt:logic (list deduce-rule ...)
                     (list choose-rule ...))])))
  
  (syntax-parse imports-stx
    [[[rel-var rhs] ...]
     #`(let ([rel-var rhs] ...) #,body)]))

;; DeclSyntax -> Bool
;; determines if the given declaration is a choice-based rule
(define (is-choose? decl-stx)  
  (syntax-parse decl-stx
    #:datum-literals (is :-)
    [(is _ (choice _ _ ...+)) #t]  ; fact
    [((is _ (choice _ _ ...+)) :- _ ...+) #t]  ; rule
    [_ #f]))

;; MutSymbolTable MutSymbolSet DeclSyntax -> RacketSyntax
(define (compile-decl arities imports decl-stx)
  ; partial application to thread around references to the symbol tables
  (let ([compile-conc ((curry compile-conc) arities imports)])
    (syntax-parse decl-stx
      #:datum-literals (:-)
      [(conc :- prems ...+)
       #:with (prem ...)
       (map ((curry compile-prem) arities imports) (attribute prems))
       #`(rt:rule #,(compile-conc #'conc) (list prem ...))]
      [conc
       #`(rt:rule #,(compile-conc #'conc) '())])))

;; MutSymbolTable MutSymbolSet ConclusionSyntax -> RacketSyntax
;; throws a compile-time error when there is a binding
;; occurrence of a variable in the conclusion, which is disallowed
(define (compile-conc arities imports conc-stx)
  ;; shadowing to disallow compiling terms with binding occurrences
  (let ([compile-term ((curry compile-term) #:can-bind #f)]
        [compile-rel-id ((curry compile-rel-id) arities imports)])
    (syntax-parse conc-stx
      #:datum-literals (is choice)
      [(is (name t ...) (choice ch ...+))
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with (comp-ch ...) (map compile-term (attribute ch))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #'(rt:rule-frag rel-var-comped (list comp-t ...) (list comp-ch ...))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #'(rt:rule-frag rel-var-comped (list comp-t ...) '())])))

;; MutSymbolTable MutSymbolSet PremiseSyntax -> RacketSyntax
(define (compile-prem arities imports prem-stx)
  (let ([compile-rel-id ((curry compile-rel-id) arities imports)])
    (syntax-parse prem-stx
      #:datum-literals (is)
      [(is (name t ...) ch)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #`(rt:fact rel-var-comped (list comp-t ...) #,(compile-term #'ch))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #'(rt:fact rel-var-comped (list comp-t ...))])))

;; MutSymbolTable MutSymbolSet Identifier Nat -> RacketSyntax
;; compiles to a reference to a bound procedure if it was imported;
;; otherwise checks the arity (if seen before, or sets arity otherwise)
;; and returns as the runtime representation of the name
(define (compile-rel-id arities imports rel-id arity)
  (if (set-member? imports (syntax->datum rel-id))
      ; sets to arity if missing from the table
      rel-id
      (let ([expected-arity (hash-ref! arities (syntax->datum rel-id) arity)])
        (unless (= arity expected-arity)
          (raise-syntax-error
           #f
           (format
            "arity mismatch: relation ~a expects ~a argument(s) but got ~a"
            (syntax->datum rel-id)
            expected-arity
            arity)
           rel-id))
        #`'#,rel-id)))

;; TermSyntax -> RacketSyntax
(define (compile-term term-stx #:can-bind [can-bind #t])
  (syntax-parse term-stx
    #:datum-literals (#%bind #%ref)
    [(#%bind x)
     (if can-bind
         #'(rt:variable 'x)
         (raise-syntax-error
          #f
          "cannot bind variables in conclusions of declarations"
          #'x))]
    [(#%ref x) #'(rt:variable 'x)]
    [x #'x]))
