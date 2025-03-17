#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse))

;; See README for the basic grammar

(syntax-spec
 (binding-class logic-nt)
 
 (host-interface/expression
  (logic d:decl ...+)
  #'(TODO))

 (nonterminal decl
   (c:conclusion)
   
   (c:conclusion (~datum :-) p:premise ...+)
   #:binding (nest p ... c))

 (nonterminal/nesting premise (nested)
   a:attribute
   #:binding (scope (import a) nested)

   ((~datum is) a:attribute t:term)
   #:binding (scope (import a) (import t) nested))

 (nonterminal/exporting attribute
   name:id
   (name:id t:term ...+)
   #:binding [(re-export t) ...])

 (nonterminal/exporting term
   (bind v:logic-nt)
   #:binding (export v)
   (ref v:logic-nt)
   e:racket-expr)

 (nonterminal conclusion
   a:attribute
   #:binding (scope (import a))
   ((~datum is) a:attribute ((~datum choice) t:term ...+))
   #:binding (scope (import a) (import t) ...)))
