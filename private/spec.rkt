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

 ;; (logic/importing (<imp> ...) (<id> ...) <decl> ...)
 (host-interface/expression
  (logic/importing ([x:rel-var e:racket-expr] ...) (ex:rel-var ...) d:decl ...)
  #:binding (scope (bind x) ... (bind ex) ... (import d) ...)
  (compile-logic #'([x e] ...) #'(ex ...) #'(d ...)))

 ;; TODO: better error messages for ill-formed imports
 ;; removing the import punning sugar resulted in an error reporting regression
 ;; TODO: logic/importing also bleeds through for bad errors
 
 ;; <decl> ::= <conclusion>                       ; fact
 ;;          | (<conclusion> :- <premise> ...+)   ; rule
 ;;          | (decls <decl> ...)                 ; nested (for macros)
 (nonterminal/exporting decl
   #:allow-extension logic-macro

   (~> ((~datum decls) (~or* (~seq #:import (imp ...))
                             (~seq))
                       d ...)
       #:with (imps ...) (or (attribute imp) #'())
       #'(#%decls (#%import imps ...) d ...))
   
   ((~datum #%decls)
    ((~datum #%import) [x:rel-var e:racket-expr] ...)
    d:decl ...)
   #:binding [(export x) ... (re-export d) ...]
   
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
 ;;                | (rkt <racket-expr>)
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
   ((~datum rkt) e:racket-expr)
   c:char)
 )

;; logic : (logic <decl> ...)
;;       | (logic #:import [<imp> ...] #:extern [id ...] <decl> ...)
(define-syntax logic
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or* (~seq #:import imports)
                (~seq))
          ;; TODO: is there a way to make it so order doesn't matter?
          (~or* (~seq #:extern externs)
                (~seq))
          ds ...)
       #:with imps (or (attribute imports) #'())
       #:with exts (or (attribute externs) #'())
       #'(logic/importing imps exts ds ...)])))
