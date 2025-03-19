#lang racket

;; maybe refine this later, yoinked from PEG class examples
(provide (all-defined-out)
         #;(for-space minidusa (all-defined-out))
         (for-syntax (all-defined-out)))

(require syntax-spec-v3
         (for-syntax syntax/parse
                     (only-in syntax-spec-v3/private/ee-lib/main lookup)
                     "compile.rkt"))

;; see README for the grammar we are incrementally working towards

(syntax-spec
 (binding-class logic-var)
 #;(extension-class logic-macro #:binding-space minidusa)

 ;; (logic <decl> ...+)
 (host-interface/expression
  (logic d:decl ...+)
  (compile-logic #'(d ...)))

 ;; <decl> ::= <conclusion>                       ; fact
 ;;          | (<conclusion> :- <premise> ...+)   ; rule
 (nonterminal decl
   (c:conclusion (~datum :-) p:premise ...+)
   #:binding (nest p ... c)
   
   c:conclusion)

 ;; <conclusion> ::= <attribute>
 ;;                | (is <attribute> (choice <logic-term> ...+))
 (nonterminal conclusion
   ;; it's important that this comes first, otherwise some things
   ;; are attempted to be parsed as logic-terms and explode
   ((~datum is) a:attribute ((~datum choice) t:logic-term ...+))
   #:binding (scope (import a) (import t) ...)

   a:attribute
   #:binding (scope (import a)))

 ;; <premise> ::= <attribute>
 ;;             | (is <attribute> <logic-term>)
 (nonterminal/nesting premise (nested)
   ((~datum is) a:attribute t:logic-term)
   #:binding (scope (import a) (import t) nested)
   
   a:attribute
   #:binding (scope (import a) nested))

 ;; <attribute> ::= (<ID> <logic-term> ...)
 (nonterminal/exporting attribute
   (name:id t:logic-term ...)
   #:binding [(re-export t) ...])

 ;; <logic-term> ::= <ID>
 ;;                | <DATUM>
 (nonterminal/exporting logic-term
   (~> v:id
       (if (lookup #'v (binding-class-predicate logic-var))
           #'(#%ref v)     ; if v has been bound as a logic-var already
           #'(#%bind v)))

   ;; TODO: is there a way to make this "private"?
   (#%bind v:logic-var)
   #:binding (export v)
   (#%ref v:logic-var)
   ;; TODO: maybe make this more expressive by allowing `racket-expr`s,
   ;; perhaps wrapped in a boundary form (for both syntax and checking)
   n:number
   b:boolean
   s:string
   ((~datum quote) s:id)
   c:char)
 )

(module+ test
  (require rackunit
           syntax/macro-testing
           (prefix-in rt: "runtime.rkt"))

  (check-equal?
   (logic
    (foo 1))
   (rt:logic (list (rt:rule (rt:rule-frag 'foo '(1) '())
                            '()))
             '()))

  (check-equal?
   (logic
    ((foo 2) :- (foo 1))
    (foo 1))
   (rt:logic (list (rt:rule (rt:rule-frag 'foo '(2) '())
                            (list (rt:fact 'foo '(1))))
                   (rt:rule (rt:rule-frag 'foo '(1) '())
                            '()))
             '()))

  (check-equal?
   (logic
    (foo "abc")
    (is (bar #t 'a) (choice 1 2 #\c)))
   (rt:logic (list (rt:rule (rt:rule-frag 'foo '("abc") '())
                            '()))
             (list (rt:rule (rt:rule-frag 'bar '(#t a) '(1 2 #\c))
                            '()))))

  (check-equal?
   (logic
    ((foo X) :- (is (bar) X) (baz)))
   (rt:logic (list (rt:rule (rt:rule-frag 'foo (list (rt:variable 'X)) '())
                            (list (rt:fact 'bar '() (rt:variable 'X))
                                  (rt:fact 'baz '()))))
             '()))

  (check-exn
   #rx"cannot bind variables in conclusions of declarations"
   (lambda ()
     (convert-compile-time-error
      (logic (foo a)))))

  (check-exn
   #rx"" ;; the actual error message is bad, we don't want to specify it
   (lambda ()
     (convert-compile-time-error
      (logic (is (bar 10))))))

  (check-equal?
   (logic
    (parent 'alice 'bob)
    (parent 'bob 'carol)

    ((ancestor X Y) :- (parent X Y))
    ((ancestor X Y) :- (parent X Z) (ancestor Z Y)))
   (rt:logic
    (list (rt:rule (rt:rule-frag 'parent '(alice bob) '()) '())
          (rt:rule (rt:rule-frag 'parent '(bob carol) '()) '())
          (rt:rule (rt:rule-frag 'ancestor
                                 (list (rt:variable 'X) (rt:variable 'Y)) '())
                   (list
                    (rt:fact 'parent
                             (list (rt:variable 'X) (rt:variable 'Y)) )))
          (rt:rule (rt:rule-frag 'ancestor
                                 (list (rt:variable 'X) (rt:variable 'Y)) '())
                   (list
                    (rt:fact 'parent
                             (list (rt:variable 'X) (rt:variable 'Z)))
                    (rt:fact 'ancestor
                             (list (rt:variable 'Z) (rt:variable 'Y))))))
    '())))
