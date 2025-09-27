#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example

(require (for-template
          racket/base
          syntax-spec-v3
          (prefix-in rt: "data.rkt"))
         syntax/parse
         syntax/id-table)

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

;; An CompilerState is a
;; (compiler-state MutSymbolTable SymbolSet SymbolSet), tracking which relation
;; names are imported, external, and arities of the rest (internally defined)
(struct compiler-state [arities imports externs])

;; this is the old compile-time function
;; ImportsSyntax IdsSyntax LogicSyntax -> RacketSyntax
(define (compile-logic imports-stx externs-stx logic-stx)
  (define arities (local-symbol-table))
  (define imports
    (apply immutable-symbol-set
           ; grabs all of the ([rel-var _] ...)s
           (map (lambda (stx-pair) (first (syntax->list stx-pair)))
                (syntax->list imports-stx))))
  (define externs (apply immutable-symbol-set (syntax->list externs-stx)))
  (define state (compiler-state arities imports externs))

  (define body
    (let ([compile-decl ((curry compile-decl) state)]
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

;; CompilerState DeclSyntax -> [ListOf RacketSyntax]
;; This returns a list so that we can expand to multiple decls in the case of
;; a `decls` block.
;; Note that DeclSyntax _does_ include the expanded relation occurrence
(define (compile-decl state decl-stx)
  ; partial application to thread around references to the symbol tables
  (let ([compile-conc ((curry compile-conc) state)]
        [compile-decl ((curry compile-decl) state)])
    (syntax-parse decl-stx
      #:datum-literals (decls :-)
      [(decls d ...) (flatten (map compile-decl (attribute d)))]
      ;; get rid of the outer ref
      [[_ (conc :- prems ...+)]
       #:with (prem ...)
       (map ((curry compile-prem) state) (attribute prems))
       (list #`(rt:rule #,(compile-conc #'conc) (list prem ...)))]
      [[_ conc]
       (list #`(rt:rule #,(compile-conc #'conc) '()))])))

;; CompilerState ConclusionSyntax -> RacketSyntax
;; throws a compile-time error when there is a binding
;; occurrence of a variable in the conclusion, which is disallowed
(define (compile-conc state conc-stx)
  ;; shadowing to disallow compiling terms with binding occurrences
  (let ([compile-term ((curry compile-term)
                       #:forbid-binds "cannot bind variables in conclusions")]
        [compile-rel-id ((curry compile-rel-id) state)]
        [raise-if-imported!
         (lambda (name)
           (when (symbol-set-member? (compiler-state-imports state) name)
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

;; CompilerState PremiseSyntax -> RacketSyntax
(define (compile-prem state prem-stx)
  (let ([compile-rel-id ((curry compile-rel-id) state)]
        [compile-term-named
         (lambda (name)
           (curry compile-term
                  #:forbid-binds
                  (and (symbol-set-member? (compiler-state-imports state) name)
                       "cannot run imported relations backwards")))])
    (syntax-parse prem-stx
      #:datum-literals (is)
      [((name t ...) is ch)
       #:with (comp-t ...)
       (map (compile-term-named #'name) (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       #`(rt:fact rel-var-comped (list comp-t ...) #,(compile-term #'ch))]
      [(name t ...)
       #:with (comp-t ...) (map compile-term (attribute t))
       #:with rel-var-comped (compile-rel-id #'name (length (attribute t)))
       (when (symbol-set-member? (compiler-state-imports state) #'name)
         (raise-syntax-error #f
                             "imported relations must be used with 'is'"
                             #'name))
       #'(rt:fact rel-var-comped (list comp-t ...))])))

;; MutSymbolTable MutSymbolSet Identifier Nat -> RacketSyntax
;; compiles to a reference to a bound procedure if it was imported;
;; otherwise checks the arity (if seen before, or sets arity otherwise)
;; and returns as the runtime representation of the name
(define (compile-rel-id state rel-id arity)
  (define rel-sym (syntax->datum rel-id))

  (when (member rel-sym RESERVED-NAMES)
    (raise-syntax-error #f "use of reserved name" rel-id))

  ;; like hash-ref! but not the thunk case for error (just set to a val)
  ;; i am surprised that something like this isn't in syntax-spec.
  (define (symbol-table-ref! table key val)
    (if (symbol-table-has-key? table key)
        (symbol-table-ref table key)
        (begin (symbol-table-set! table key val #:allow-overwrite? #t)
               val)))

  (cond [(symbol-set-member? (compiler-state-imports state) rel-id)
         rel-id]
        [(symbol-set-member? (compiler-state-externs state) rel-id)
         #`'#,rel-id]
        [else
         (define expected-arity
           ; will add to the table if missing, which will pass equality check
           (symbol-table-ref! (compiler-state-arities state) rel-id arity))
         (unless (= arity expected-arity)
          (raise-syntax-error
           #f
           (format
            "arity mismatch: relation ~a expects ~a argument(s) but got ~a"
            rel-sym
            expected-arity
            arity)
           rel-id))
         #`#'#,rel-id]))

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
