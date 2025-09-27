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

 ;; (logic/importing <imps> (<id> ...) <decl> ...)
 (host-interface/expression
  (logic/importing i:imps (ex:rel-var ...) d:decl ...)
  #:binding (nest i (scope (bind ex) ... (import d) ...))
  (compile-logic #'i #'(ex ...) #'(d ...)))

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
