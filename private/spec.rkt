#lang racket

;; maybe refine this later, yoinked from PEG class examples
(provide (except-out (all-defined-out)
                     logic/importing)
         (for-syntax (all-defined-out)))

(require syntax-spec-v3
         (for-syntax syntax/parse
                     (only-in syntax-spec-v3/private/ee-lib/main lookup)
                     "compile.rkt")
         (prefix-in rt: "data.rkt"))

;; see README for the grammar we are incrementally working towards

(syntax-spec
 (binding-class logic-var)
 (binding-class rel-var)
 (extension-class logic-macro #:binding-space minidusa)

 ;; (logic/importing <imps> <decl> ...)
 (host-interface/expression
  (logic/importing i:imps d:decl ...)
  #:binding (nest i (scope (import d) ...))
  (compile-logic #'i #'(d ...)))

 ;; <imps> ::= (<imp> ...)
 ;; <imp>  ::= x:racket-var
 ;;          | [x:id e:racket-expr]
 (nonterminal/nesting imps (nested)
   ([x:rel-var e:racket-expr] ...)
   #:binding [e ... (scope (bind x) ... nested)]

   ;; If we have a racket-var, that is shorthand for binding it to
   ;; a rel-var with the same name, so we expand accordingly.
   ;; This case comes after the core case so that it only matches
   ;; when there are shorthands that need to expand.
   (~> ((~or* (~and x:id (~bind [e #'x])) [x:id e:expr]) ...)
       #'([x e] ...)))

 ;; <decl> ::= <conclusion>                       ; fact
 ;;          | (<conclusion> :- <premise> ...+)   ; rule
 ;;          | (decls <decl> ...)                 ; nested (for macros)
 (nonterminal/exporting decl
   #:allow-extension logic-macro

   ((~datum decls) d:decl ...)
   #:binding [(re-export d) ...]
   
   (~> (~and d
             ;; drill down to find the relation name, then extract for binding
             ;; we can always do this, since we don't expand macros here
             (~or (r:id _ ...)
                  ((r:id _ ...) (~datum :-) _ ...+)
                  ((r:id _ ...) (~or (~datum is) (~datum is?)) {_ ...+})
                  (((r:id _ ...) (~or (~datum is) (~datum is?)) {_ ...+})
                   (~datum :-) _ ...+)))

       (if (lookup #'r (binding-class-predicate rel-var))
           #'[(#%ref/rel r) d]     ; if r has been bound as a rel-var already
           #'[(#%bind/rel r) d]))
   
   [r:rel (c:conclusion (~datum :-) p:premise ...+)]
   #:binding [(re-export r) (nest p ... c)]

   [r:rel c:conclusion]
   #:binding (re-export r))

 
 (nonterminal/exporting rel
   ((~datum #%bind/rel) r:rel-var)
   #:binding (export r)

   ((~datum #%ref/rel) r:rel-var))

 ;; <conclusion> ::= <attr>
 ;;                | (<attr> is {<logic-term> ...+})
 ;;                | (<attr> is? {<logic-term> ...+})
 (nonterminal conclusion
   ;; it's important that this comes first, otherwise some things
   ;; are attempted to be parsed as logic-terms and explode
   (a:attr (~datum is) {t:logic-term ...+})
   #:binding (scope (import a) (import t) ...)

   (a:attr (~datum is?) {t:logic-term ...+})
   #:binding (scope (import a) (import t) ...)

   a:attr
   #:binding (scope (import a)))

 ;; <premise> ::= <attr>
 ;;             | (<attr> is <logic-term>)
 (nonterminal/nesting premise (nested)
   (a:attr (~datum is) t:logic-term)
   #:binding (scope (import a) (import t) nested)

   a:attr
   #:binding (scope (import a) nested))

 ;; <attr> ::= (<ID> <logic-term> ...)
 (nonterminal/exporting attr   
   (name:rel-var t:logic-term ...)
   #:binding [(re-export t) ...])

 ;; <logic-term> ::= <ID>
 ;;                | <DATUM>
 (nonterminal/exporting logic-term
   (~> v:id
       (if (lookup #'v (binding-class-predicate logic-var))
           #'(#%ref v)     ; if v has been bound as a logic-var already
           #'(#%bind v)))

   ;; TODO: is there a way to make this "private"?
   ((~datum #%bind) v:logic-var)
   #:binding (export v)
   ((~datum #%ref) v:logic-var)
   ;; TODO: maybe make this more expressive by allowing `racket-expr`s,
   ;; perhaps wrapped in a boundary form (for both syntax and checking)
   n:number
   b:boolean
   s:string
   ((~datum quote) s:id)
   c:char)
 )

;; logic : (logic <decl> ...)
;;       | (logic #:import [<imp> ...] <decl> ...)
(define-syntax logic
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or* (~seq #:import imports)
                (~seq))
          ds ...)
       #:with imps (or (attribute imports) #'())
       #'(logic/importing imps ds ...)])))

(module+ test
  (require rackunit
           syntax/macro-testing
           (prefix-in rt: "runtime.rkt"))

  (check-equal?
   (logic
     (foo 1))
   (rt:program (list (rt:rule (rt:rule-frag 'foo '(1) '() #f)
                              '()))
               '()))
  
  (check-equal?
   (logic
     ((foo 2) :- (foo 1))
     (foo 1))
   (rt:program (list (rt:rule (rt:rule-frag 'foo '(2) '() #f)
                              (list (rt:fact 'foo '(1))))
                     (rt:rule (rt:rule-frag 'foo '(1) '() #f)
                              '()))
               '()))

  (check-equal?
   (logic
     (foo "abc")
     ((bar #t 'a) is {1 2 #\c}))
   (rt:program (list (rt:rule (rt:rule-frag 'foo '("abc") '() #f)
                              '()))
               (list (rt:rule (rt:rule-frag 'bar '(#t a) '(1 2 #\c) #f)
                              '()))))

  ;; we disallow binding relation variables on RHS of :-
  (check-exn
   #rx"not bound as rel-var"
   (lambda ()
     (convert-compile-time-error
      (logic ((foo X) :- ((bar) is X) (baz))))))
  
  (check-equal?
   (logic
     (bar)
     ((foo X) :- ((bar) is X) (baz))
     (baz))
   (rt:program (list (rt:rule (rt:rule-frag 'bar '() '() #f) '())
                     (rt:rule (rt:rule-frag 'foo (list (rt:variable 'X)) '() #f)
                              (list (rt:fact 'bar '() (rt:variable 'X))
                                    (rt:fact 'baz '())))
                     (rt:rule (rt:rule-frag 'baz '() '() #f) '()))
               '()))

  ;; some error cases

  (check-exn
   #rx"cannot bind variables in conclusions"
   (lambda ()
     (convert-compile-time-error
      (logic (foo a)))))

  (check-exn
   #rx"1 argument\\(s\\) but got 2"
   (lambda ()
     (convert-compile-time-error
      (logic
        (foo 1)
        (foo 1 2)))))

  (check-exn
   ; this error message isn't good, but it's from syntax-spec
   #rx""
   (lambda ()
     (convert-compile-time-error
      (logic (is (bar 10))))))

  (check-exn
   #rx"use of reserved name"
   (lambda ()
     (convert-compile-time-error
      (logic (is 10)))))

  ;; larger examples

  (check-equal?
   (logic
     (decls (parent 'alice 'bob)
            (decls (parent 'bob 'carol))
            (decls))

     (decls ((ancestor X Y) :- (parent X Y)))
     ((ancestor X Y) :- (parent X Z) (ancestor Z Y)))
   (rt:program
    (list (rt:rule (rt:rule-frag 'parent '(alice bob) '() #f) '())
          (rt:rule (rt:rule-frag 'parent '(bob carol) '() #f) '())
          (rt:rule (rt:rule-frag 'ancestor
                                 (list (rt:variable 'X) (rt:variable 'Y))
                                 '()
                                 #f)
                   (list
                    (rt:fact 'parent
                             (list (rt:variable 'X) (rt:variable 'Y)) )))
          (rt:rule (rt:rule-frag 'ancestor
                                 (list (rt:variable 'X) (rt:variable 'Y)) '()
                                 #f)
                   (list
                    (rt:fact 'parent
                             (list (rt:variable 'X) (rt:variable 'Z)))
                    (rt:fact 'ancestor
                             (list (rt:variable 'Z) (rt:variable 'Y))))))
    '()))

  (check-equal?
   (logic
     ;; these are unbound relation names, because this is a placeholder example
     ;; this is a way to declare that
     ((region R) :- (region R))
     ((adjacent R S) :- (adjacent R S))
     
     (((terrain R) is {'mountain 'forest 'ocean}) :- (region R))
     (((terrain R) is {'forest 'ocean})
      :-
      (adjacent R S) ((terrain S) is 'ocean)))
   (rt:program
    (list
     (rt:rule
      (rt:rule-frag 'region (list (rt:variable 'R)) '() #f)
      (list (rt:fact 'region (list (rt:variable 'R)))))
     (rt:rule
      (rt:rule-frag 'adjacent (list (rt:variable 'R) (rt:variable 'S)) '() #f)
      (list (rt:fact 'adjacent (list (rt:variable 'R) (rt:variable 'S))))))
    (list
     (rt:rule
      (rt:rule-frag 'terrain
                    (list (rt:variable 'R))
                    '(mountain forest ocean) #f)
      (list (rt:fact 'region (list (rt:variable 'R)))))
     (rt:rule
      (rt:rule-frag 'terrain (list (rt:variable 'R)) '(forest ocean) #f)
      (list
       (rt:fact 'adjacent (list (rt:variable 'R) (rt:variable 'S)))
       (rt:fact 'terrain (list (rt:variable 'S)) 'ocean))))))

  ;; importing tests

  (check-equal?
   (logic #:import ([a add1])
     ((foo) :- ((a 0) is 1)))
   (rt:program
    (list (rt:rule (rt:rule-frag 'foo '() '() #f)
                   (list (rt:fact add1 '(0) 1))))
    '()))

  (check-equal?
   (logic #:import ([p +])
     ((foo) :- ((p 1 2) is 3))
     ((bar X) :- ((p 1 2 3) is X)))
   (rt:program
    (list (rt:rule (rt:rule-frag 'foo '() '() #f)
                   (list (rt:fact + '(1 2) 3)))
          (rt:rule (rt:rule-frag 'bar (list (rt:variable 'X)) '() #f)
                   (list (rt:fact + '(1 2 3) (rt:variable 'X)))))
    '()))
  
  (check-equal?
   (logic #:import [add1]
     ((foo) :- ((add1 0) is 1)))
   (rt:program
    (list (rt:rule (rt:rule-frag 'foo '() '() #f)
                   (list (rt:fact add1 '(0) 1))))
    '()))

  (check-equal?
   (logic #:import ([add1 add1])
     (foo 1)
     ((bar) :- (foo X) ((add1 X) is 2)))
   (rt:program
    (list (rt:rule (rt:rule-frag 'foo '(1) '() #f) '())
          (rt:rule (rt:rule-frag 'bar '() '() #f)
                   (list (rt:fact 'foo (list (rt:variable 'X)))
                         (rt:fact add1 (list (rt:variable 'X)) 2))))
    '()))

  ;; errors with imports
  (check-exn
   #rx"imported relations cannot appear in conclusions"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   (add1 0)))))

  (check-exn
   #rx"imported relations cannot appear in conclusions"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   ((add1 0) is {1})))))

  (check-exn
   #rx"imported relations must be used with 'is'"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   ((foo) :- (add1 0))))))

  (check-exn
   #rx"identifier already defined"
   (lambda ()
     (convert-compile-time-error (logic #:import ([x add1] [x sub1])))))
  
  (check-exn
   #rx"cannot run imported relations backwards"
   (lambda ()
     (convert-compile-time-error
      (logic #:import [add1]
        ((foo X) :- ((add1 X) is 2))))))

  ;; is? rules
  (check-equal?
   (logic
     (((a) is? {1}) :- ((a) is 1))
     ((a) is {1})
     ((a) is? {2 3}))
   (rt:program
    (list (rt:rule (rt:rule-frag 'a '() '(1) #f) '()))
    (list
     (rt:rule (rt:rule-frag 'a '() '(1) #t) (list (rt:fact 'a '() 1)))
     (rt:rule (rt:rule-frag 'a '() '(2 3) #t) '()))))

  ;; is? errors
  (check-exn
   #rx"expected attr" ;; FIXME: this is bad
   (lambda ()
     (convert-compile-time-error
      (logic
        ((foo) :- ((foo) is? 1))))))
  )
