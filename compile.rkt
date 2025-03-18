#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example

(require (for-template
          racket/base
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
#;(rt:logic (list (rt:rule (rt:rule-frag 'foo (list (variable 'X)) '())
                           (list (fact 'bar '() (variable 'X))
                                 (fact 'baz '()))))
            '())

;; this is the old compile-time function
;; LogicSyntax -> RacketSyntax
(define (compile-logic logic-stx)
  (syntax-parse logic-stx
    [(d ...+)
     ;; like define/syntax-parse, but shorter
     #:with (deduce-rule ...)
     (map compile-decl (filter (negate is-choose?) (attribute d)))

     #:with (choose-rule ...)
     (map compile-decl (filter is-choose? (attribute d)))
     
     #'(rt:logic (list deduce-rule ...)
                 (list choose-rule ...))]))

;; DeclSyntax -> Bool
;; determines if the given declaration is a choice-based rule
(define (is-choose? decl-stx)  
  (syntax-parse decl-stx
    #:datum-literals (is :-)
    [(is _ _) #t]  ; fact
    [((is _ _) :- _ ...+) #t]  ; rule
    [_ #f]))

;; DeclSyntax -> RacketSyntax
(define (compile-decl decl-stx)
  (syntax-parse decl-stx
    #:datum-literals (:-)
    [(conc :- prems ...+)
     #:with (prem ...) (map compile-prem (attribute prems))
     #`(rt:rule #,(compile-conc #'conc) (list prem ...))]
    [conc
     #`(rt:rule #,(compile-conc #'conc) '())]))

;; ConclusionSyntax -> RacketSyntax
;; throws a compile-time error when there is a binding
;; occurrence of a variable in the conclusion, which is disallowed
(define (compile-conc conc-stx)
  ;; shadowing to disallow compiling terms with binding occurrences
  (let ([compile-term ((curry compile-term) #:can-bind #f)])
    (syntax-parse conc-stx
      #:datum-literals (is choice)
      [(is (name t ...) (choice ch ...+))
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with (comp-ch ...) (map compile-term (attribute ch))
       #'(rt:rule-frag 'name (list comp-t ...) (list comp-ch ...))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #'(rt:rule-frag 'name (list comp-t ...) '())])))

;; PremiseSyntax -> RacketSyntax
(define (compile-prem prem-stx)
  (syntax-parse prem-stx
    #:datum-literals (is)
    [(is (name t ...) ch)
     #:with (comp-t ...) (map compile-term (attribute t))
     #`(rt:fact 'name (list comp-t ...) #,(compile-term #'ch))]
    [(name t ...)
     #:with (comp-t ...) (map compile-term (attribute t))
     #'(rt:fact 'name (list comp-t ...))]))

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