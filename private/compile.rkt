#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example

(require (for-template
          racket/base
          syntax-spec-v3
          (prefix-in rt: "data.rkt"))
         syntax/parse)

(define RESERVED-NAMES '(is is? :- decls rkt))

;; EXAMPLE EXPANSIONS!
#;(logic
    (foo 1))
;; =>
#;(rt:program (list (rt:rule (rt:rule-frag 'foo '(1) '())
                             '()))
              '())

#;(logic
    ((foo 2) :- (foo 1))
    (foo 1))
;; =>
#;(rt:program (list (rt:rule (rt:rule-frag 'foo '(2) '())
                             (list (rt:fact 'foo '(1))))
                    (rt:rule (rt:rule-frag 'foo '(1) '())
                             '()))
              '())

#;(logic
    (foo 1)
    ((bar 10 11) is {1 2 3}))
;; =>
#;(rt:program (list (rt:rule (rt:rule-frag 'foo '(1) '())
                             '()))
              (list (rt:rule (rt:rule-frag 'bar '(10 11) '(1 2 3))
                             '())))

#;(logic
    ((foo X) :- ((bar) is X) (baz)))
;; =>
#;(rt:program (list (rt:rule (rt:rule-frag 'foo (list (rt:variable 'X)) '())
                             (list (rt:fact 'bar '() (rt:variable 'X))
                                   (rt:fact 'baz '()))))
              '())

#;(logic #:import ([a add1])
    ((foo) :- ((a 0) is 1)))
;; =>
#;(let ([a add1])
    (rt:program
     (list (rt:rule (rt:rule-frag 'foo '() '())
                    (list (rt:fact a '(0) 1))))
     '()))

#;(logic #:import (add1)
    (foo 1)
    ((bar) :- (foo X) ((add1 X) is 2)))
;; =>
#;(let ([add1 add1])
    (rt:program
     (list (rt:rule (rt:rule-frag 'foo '(1) '()) '())
           (rt:rule (rt:rule-frag 'bar '() '())
                    (list (rt:fact 'foo (list (rt:variable 'X)))
                          (rt:fact add1 (list (rt:variable 'X)) 2))))
     '()))

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
    (let ([compile-decl ((curry compile-decl) rel-arities imported-rel-vars)]
          [logic-stx (flatten-decls logic-stx)])
      (syntax-parse logic-stx
        [(d ...)
         ;; like define/syntax-parse, but shorter
         #:with (deduce-rule ...)
         (flatten (map compile-decl
                       (filter (negate is-choose?) (attribute d))))

         #:with (choose-rule ...)
         (flatten (map compile-decl
                       (filter is-choose? (attribute d))))

         #'(rt:program (list deduce-rule ...)
                       (list choose-rule ...))])))

  (syntax-parse imports-stx
    [[[rel-var rhs] ...]
     #`(let ([rel-var rhs] ...) #,body)]))

;; flatten-decls : LogicSyntax -> LogicSyntax
;; flattens all `decls` blocks into a single list of decls at the top level
(define (flatten-decls logic-stx)
  (syntax-parse logic-stx
    #:datum-literals (decls)
    [() #'()]
    [((decls inner ...) rest ...)
     (flatten-decls #'(inner ... rest ...))]
    [(d rest ...) #`(d #,@(flatten-decls #'(rest ...)))]))

;; DeclSyntax -> Bool
;; determines if the given declaration is a choice-based rule
;; Note that DeclSyntax _does_ include the expanded relation occurrence
(define (is-choose? decl-stx)
  (syntax-parse decl-stx
    #:datum-literals (is is? :-)
    ; we require two choices since a choice of one is
    ; not really a choice
    [[_ (_ is {_ _ ...+})] #t]
    [[_ ((_ is {_ _ ...+}) :- _ ...+)] #t]
    ;; an `is?` rule is always a choice (with not firing)
    [[_ (_ is? {_ ...+})] #t]
    [[_ ((_ is? {_ ...+}) :- _ ...+)] #t]
    [_ #f]))

;; MutSymbolTable MutSymbolSet DeclSyntax -> [ListOf RacketSyntax]
;; This returns a list so that we can expand to multiple decls in the case of
;; a `decls` block.
;; Note that DeclSyntax _does_ include the expanded relation occurrence
(define (compile-decl arities imports decl-stx)
  ; partial application to thread around references to the symbol tables
  (let ([compile-conc ((curry compile-conc) arities imports)]
        [compile-decl ((curry compile-decl) arities imports)])
    (syntax-parse decl-stx
      #:datum-literals (decls :-)
      [(decls d ...) (flatten (map compile-decl (attribute d)))]
      ;; get rid of the outer ref
      [[_ (conc :- prems ...+)]
       #:with (prem ...)
       (map ((curry compile-prem) arities imports) (attribute prems))
       (list #`(rt:rule #,(compile-conc #'conc) (list prem ...)))]
      [[_ conc]
       (list #`(rt:rule #,(compile-conc #'conc) '()))])))

;; MutSymbolTable MutSymbolSet ConclusionSyntax -> RacketSyntax
;; throws a compile-time error when there is a binding
;; occurrence of a variable in the conclusion, which is disallowed
(define (compile-conc arities imports conc-stx)
  ;; shadowing to disallow compiling terms with binding occurrences
  (let ([compile-term ((curry compile-term)
                       #:forbid-binds "cannot bind variables in conclusions")]
        [compile-rel-id ((curry compile-rel-id) arities imports)]
        [raise-if-imported!
         (lambda (name)
           (when (set-member? imports (syntax->datum name))
             (raise-syntax-error
              #f
              "imported relations cannot appear in conclusions"
              name)))])
    (syntax-parse conc-stx
      #:datum-literals (is is?)
      [((name t ...) (~or is (~and is? is??)) {ch ...+})
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with (comp-ch ...) (map compile-term (attribute ch))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       (raise-if-imported! #'name)
       #`(rt:rule-frag rel-var-comped
                       (list comp-t ...)
                       (list comp-ch ...)
                       ; if is?? is bound, we have an `is?` rule, so we pass #t
                       #,(and (attribute is??) #t))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       (raise-if-imported! #'name)
       #'(rt:rule-frag rel-var-comped (list comp-t ...) '() #f)])))

;; MutSymbolTable MutSymbolSet PremiseSyntax -> RacketSyntax
(define (compile-prem arities imports prem-stx)
  (let ([compile-rel-id ((curry compile-rel-id) arities imports)]
        [compile-term-named
         (lambda (name)
           (curry compile-term
                  #:forbid-binds
                  (and (set-member? imports name)
                       "cannot run imported relations backwards")))])
    (syntax-parse prem-stx
      #:datum-literals (is)
      [((name t ...) is ch)
       #:with (comp-t ...)
       (map (compile-term-named (syntax->datum #'name)) (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #`(rt:fact rel-var-comped (list comp-t ...) #,(compile-term #'ch))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       (when (set-member? imports (syntax->datum #'name))
         (raise-syntax-error #f
                             "imported relations must be used with 'is'"
                             #'name))
       #'(rt:fact rel-var-comped (list comp-t ...))])))

;; MutSymbolTable MutSymbolSet Identifier Nat -> RacketSyntax
;; compiles to a reference to a bound procedure if it was imported;
;; otherwise checks the arity (if seen before, or sets arity otherwise)
;; and returns as the runtime representation of the name
(define (compile-rel-id arities imports rel-id arity)
  (define rel-sym (syntax->datum rel-id))

  (when (member rel-sym RESERVED-NAMES)
    (raise-syntax-error #f "use of reserved name" rel-id))

  (if (set-member? imports rel-sym)
      ; sets to arity if missing from the table -> will be equal
      rel-id
      (let ([expected-arity (hash-ref! arities rel-sym arity)])
        (unless (= arity expected-arity)
          (raise-syntax-error
           #f
           (format
            "arity mismatch: relation ~a expects ~a argument(s) but got ~a"
            rel-sym
            expected-arity
            arity)
           rel-id))
        #`#'#,rel-id)))

;; TermSyntax -> RacketSyntax
(define (compile-term term-stx #:forbid-binds [message #f])
  (syntax-parse term-stx
    #:datum-literals (#%bind #%ref rkt)
    [(#%bind x)
     (if (not message)
         #'(rt:variable 'x)
         (raise-syntax-error #f message #'x))]
    [(#%ref x) #'(rt:variable 'x)]
    [(rkt e) #'e]
    [x #'x]))
