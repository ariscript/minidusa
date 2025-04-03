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
  (define rel-arities (local-symbol-table))
  (define imported-rel-vars (local-symbol-set))
  ; add all imported symbols; later we use this to compile attibutes
  (for ([stx-pair (syntax->list imports-stx)])
    (syntax-parse stx-pair
      [[rel-var _]
       (symbol-set-add! imported-rel-vars (attribute rel-var))]))

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
        [compile-rel-var ((curry compile-rel-var) arities imports)])
    (syntax-parse conc-stx
      #:datum-literals (is choice)
      [(is (name t ...) (choice ch ...+))
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with (comp-ch ...) (map compile-term (attribute ch))
       #:with rel-var-comped (compile-rel-var #'name (length (attribute t)))
       #'(rt:rule-frag rel-var-comped (list comp-t ...) (list comp-ch ...))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-var #'name (length (attribute t)))
       #'(rt:rule-frag rel-var-comped (list comp-t ...) '())])))

;; MutSymbolTable MutSymbolSet PremiseSyntax -> RacketSyntax
(define (compile-prem arities imports prem-stx)
  (let ([compile-rel-var ((curry compile-rel-var) arities imports)])
    (syntax-parse prem-stx
      #:datum-literals (is)
      [(is (name t ...) ch)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-var #'name (length (attribute t)))
       #`(rt:fact rel-var-comped (list comp-t ...) #,(compile-term #'ch))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-var #'name (length (attribute t)))
       #'(rt:fact rel-var-comped (list comp-t ...))])))

;; MutSymbolTable MutSymbolSet RelVarSyntax Nat -> RacketSyntax
;; compiles to a reference to a bound procedure if it was imported;
;; otherwise checks the arity (if seen before, or sets arity otherwise)
;; and returns as the runtime representation of the name
(define (compile-rel-var arities imports rv arity)
  (cond
    [(symbol-set-member? imports rv) rv]
    [(symbol-table-has-key? arities rv)
     (define expected-arity (symbol-table-ref arities rv))
     (unless (= arity expected-arity)
       (raise-syntax-error
        #f
        (format "arity mismatch: relation ~a expects arity ~a but got ~a"
                rv
                expected-arity
                arity)
        rv))
     ; TODO: this is the spot where we could syntax-quote this instead
     ; my face when #`#'#,    (#_#)b
     ; ALSO: we can check to make sure that these aren't evil reserved ids
     #`'#,rv]
    [else
     (symbol-table-set! arities rv arity)
     #`'#,rv]))

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
