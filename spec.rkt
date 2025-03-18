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
   ((~datum quote) s:id)
   ;; s:string ;; if this gets uncommented, then (bar 10) parses as logic-term
   ;; how can we have something that parses symbols, but not other stuff?
   c:char)
 )

;; some examples: these (surprisingly) actually work!
(logic
 (baz)
 (foo 1)
 ((foo 2) :- (foo 1))
 ((foo 0) :- (foo 2) (foo 1))

 ;; correctly fails to parse, even if message isn't great:
 ;; (is (bar 10))

 ;; not sure if this is actually good syntax...
 (is (bar 10) (choice 1 2 3))
 ((is (bar 20) (choice 4 5 6)) :- (foo 0) (is (foo 1) 45))

 ;; this parses but doesn't scope check.
 ;; (baz x)
 
 ((baz X) :- (baz X) (qux X))
 ((abc X X) :- (abc X X))
 ((abc X X) :- (abc Y X))

 ;; like above, this is statically rejected
 ;; ((abc Y Y) :- (abc X X))
 )

(logic
 ((foo X) :- (is (bar) X) (baz)))